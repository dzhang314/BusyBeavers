module BusyBeavers

################################################################################

export TapeSymbol, DEFAULT_SYMBOL

struct TapeSymbol
    value::Bool
end

const DEFAULT_SYMBOL = TapeSymbol(false)

################################################################################

export TapeDirection, LEFT, RIGHT

struct TapeDirection
    value::Bool
end

const LEFT = TapeDirection(false)
const RIGHT = TapeDirection(true)

################################################################################

export State, DEFAULT_STATE, HALT, state_range

struct State
    value::UInt8
end

const DEFAULT_STATE = State(0x01)
const HALT = State(0x00)

function state_range(n::UInt8)
    return Iterators.map(State, DEFAULT_STATE.value:n)
end

################################################################################

export TransitionRule, get_symbol, get_direction, get_state,
    NULL_RULE, HALT_RULE, INITIAL_RULE, can_halt

struct TransitionRule
    data::UInt8
end

const SYMBOL_BIT::UInt8 = 0x80
const DIRECTION_BIT::UInt8 = 0x40
const STATE_MASK::UInt8 = 0x3F

function TransitionRule(
    symbol::TapeSymbol, direction::TapeDirection, state::State
)
    data = zero(UInt8)
    if symbol.value
        data |= SYMBOL_BIT
    end
    if direction.value
        data |= DIRECTION_BIT
    end
    @assert state.value <= STATE_MASK
    data |= state.value
    return TransitionRule(data)
end

function get_symbol(rule::TransitionRule)
    return TapeSymbol(!iszero(rule.data & SYMBOL_BIT))
end

function get_direction(rule::TransitionRule)
    return TapeDirection(!iszero(rule.data & DIRECTION_BIT))
end

function get_state(rule::TransitionRule)
    return State(rule.data & STATE_MASK)
end

const NULL_RULE = TransitionRule(0xFF)
const HALT_RULE = TransitionRule(TapeSymbol(true), RIGHT, HALT)
const INITIAL_RULE = TransitionRule(
    TapeSymbol(true),
    RIGHT,
    State(DEFAULT_STATE.value + one(DEFAULT_STATE.value))
)

@inline function can_halt(rule::TransitionRule)
    return (rule == NULL_RULE) | (rule == HALT_RULE)
end

################################################################################

export TransitionTable, set_rule, has_rule, count_rule, replace_rule,
    to_string, is_duplicate, distinct_states

struct TransitionTable{N}
    data::NTuple{N,NTuple{2,TransitionRule}}
end

function TransitionTable{N}() where {N}
    return TransitionTable{N}(ntuple(_ -> ntuple(_ -> NULL_RULE, 2), N))
end

@inline function Base.getindex(
    table::TransitionTable{N}, state::State
) where {N}
    # optimized, zero allocations, clean assembly
    # @assert DEFAULT_STATE.value <= state.value <= UInt8(N)
    return @inbounds table.data[state.value]
end

@inline function set_rule(
    table::TransitionTable{N}, symbol::TapeSymbol, state::State,
    rule::TransitionRule
) where {N}
    # optimized, zero allocations, clean assembly
    # @assert DEFAULT_STATE.value <= state.value <= UInt8(N)
    # @assert HALT.value <= get_state(rule).value <= UInt8(N)
    a, b = table[state]
    return TransitionTable{N}(Base._setindex(
        symbol.value ? (a, rule) : (rule, b),
        state.value,
        table.data...
    ))
end

function has_rule(table::TransitionTable{N}, rule::TransitionRule) where {N}
    for state in state_range(UInt8(N))
        a, b = table[state]
        if (a == rule) | (b == rule)
            return true
        end
    end
    return false
end

function count_rule(table::TransitionTable{N}, rule::TransitionRule) where {N}
    result = 0
    for state in state_range(UInt8(N))
        a, b = table[state]
        if a == rule
            result += 1
        end
        if b == rule
            result += 1
        end
    end
    return result
end

function replace_rule(
    table::TransitionTable{N}, old::TransitionRule, new::TransitionRule
) where {N}
    return TransitionTable{N}(ntuple(
        i -> ntuple(
            j -> ifelse(table.data[i][j] == old, new, table.data[i][j]),
            2
        ),
        N
    ))
end

function to_string(table::TransitionTable{N}) where {N}
    result = Vector{UInt8}(undef, 6 * N)
    for i = DEFAULT_STATE.value:UInt8(N)
        a, b = table[State(i)]
        if a == NULL_RULE
            result[6*i-5] = UInt8('?')
            result[6*i-4] = UInt8('?')
            result[6*i-3] = UInt8('?')
        else
            result[6*i-5] = get_symbol(a).value ? UInt8('1') : UInt8('0')
            result[6*i-4] = get_direction(a).value ? UInt8('R') : UInt8('L')
            if get_state(a) == HALT
                result[6*i-3] = 'H'
            else
                result[6*i-3] = UInt8('A' + get_state(a).value - 1)
            end
        end
        if b == NULL_RULE
            result[6*i-2] = UInt8('?')
            result[6*i-1] = UInt8('?')
            result[6*i-0] = UInt8('?')
        else
            result[6*i-2] = get_symbol(b).value ? UInt8('1') : UInt8('0')
            result[6*i-1] = get_direction(b).value ? UInt8('R') : UInt8('L')
            if get_state(b) == HALT
                result[6*i-0] = 'H'
            else
                result[6*i-0] = UInt8('A' + get_state(b).value - 1)
            end
        end
    end
    return String(result)
end

function is_duplicate(
    table::TransitionTable{N}, state::State, special::State
) where {N}
    @assert DEFAULT_STATE.value <= state.value <= UInt8(N)
    @assert DEFAULT_STATE.value <= special.value <= UInt8(N)
    for predecessor in state_range(state.value - one(state.value))
        if (predecessor != special) && (table[state] == table[predecessor])
            return true
        end
    end
    return false
end

function distinct_states(table::TransitionTable{N}, special::State) where {N}
    @assert N < 64
    @assert DEFAULT_STATE.value <= special.value <= UInt8(N)
    result = zero(UInt64)
    for state in state_range(UInt8(N))
        if (state == special) || !is_duplicate(table, state, special)
            result |= one(UInt64) << state.value
        end
    end
    return result
end

function all_states_defined(table::TransitionTable{N}, special::State) where {N}
    for state in state_range(UInt8(N))
        if state != special
            a, b = table[state]
            if (a == NULL_RULE) && (b == NULL_RULE)
                return false
            end
        end
    end
    return true
end

################################################################################

export TuringMachine, get_rule, set_rule, has_halted, has_transition, can_halt,
    step!, push_successors!

struct TuringMachine{N}
    transition_table::TransitionTable{N}
    state::Array{State,0}
    position::Array{Int,0}
    tape::BitSet
end

function TuringMachine(transition_table::TransitionTable{N}) where {N}
    return TuringMachine{N}(
        transition_table, fill(DEFAULT_STATE), fill(0), BitSet()
    )
end

function TuringMachine{N}(transition_table::TransitionTable{N}) where {N}
    return TuringMachine{N}(
        transition_table, fill(DEFAULT_STATE), fill(0), BitSet()
    )
end

function Base.:(==)(x::TuringMachine{N}, y::TuringMachine{N}) where {N}
    if x.transition_table != y.transition_table
        return false
    end
    if x.state[] != y.state[]
        return false
    end
    if x.position[] != y.position[]
        return false
    end
    if x.tape != y.tape
        return false
    end
    return true
end

@inline function Base.hash(tm::TuringMachine{N}, h::UInt) where {N}
    # optimized, zero allocations, clean assembly
    @inline h = hash(tm.transition_table, h)
    @inline h = hash(tm.state[].value, h)
    @inline h = hash(tm.position[], h)
    @inline h = hash(tm.tape, h)
    return h
end

function Base.copy(tm::TuringMachine{N}) where {N}
    return TuringMachine{N}(
        tm.transition_table,
        copy(tm.state),
        copy(tm.position),
        copy(tm.tape)
    )
end

function get_rule(tm::TuringMachine{N}) where {N}
    row = tm.transition_table[tm.state[]]
    return (tm.position[] in tm.tape) ? row[2] : row[1]
end

function set_rule(tm::TuringMachine{N}, rule::TransitionRule) where {N}
    @assert get_rule(tm) == NULL_RULE
    @assert HALT.value <= get_state(rule).value <= UInt8(N)
    return TuringMachine{N}(
        set_rule(
            tm.transition_table,
            TapeSymbol(tm.position[] in tm.tape),
            tm.state[],
            rule
        ),
        copy(tm.state),
        copy(tm.position),
        copy(tm.tape)
    )
end

function has_halted(tm::TuringMachine{N}) where {N}
    return tm.state[] == HALT
end

function has_transition(tm::TuringMachine{N}) where {N}
    return get_rule(tm) != NULL_RULE
end

@inline function can_halt(tm::TuringMachine{N}) where {N}
    # optimized, zero allocations, clean assembly
    # @assert N < 64
    if has_halted(tm)
        return true
    end
    seen = zero(UInt64)
    stack = zero(UInt64)
    stack |= one(UInt64) << tm.state[].value
    while !iszero(stack)
        next = trailing_zeros(stack)
        bit = one(UInt64) << next
        stack &= ~bit
        if iszero(seen & bit)
            a, b = @inbounds tm.transition_table.data[next]
            if can_halt(a) | can_halt(b)
                return true
            end
            seen |= bit
            stack |= one(UInt64) << get_state(a).value
            stack |= one(UInt64) << get_state(b).value
        end
    end
    return false
end

function step!(tm::TuringMachine{N}) where {N}
    @assert !has_halted(tm)
    @assert has_transition(tm)
    rule = get_rule(tm)
    Base._setint!(tm.tape, tm.position[], get_symbol(rule).value)
    if get_direction(rule).value
        tm.position[] += one(eltype(tm.position))
    else
        tm.position[] -= one(eltype(tm.position))
    end
    tm.state[] = get_state(rule)
    return tm
end

function push_successors!(
    result::Vector{TuringMachine{N}}, tm::TuringMachine{N},
    symbol::TapeSymbol, direction::TapeDirection, states::UInt64
) where {N}
    while !iszero(states)
        value = trailing_zeros(states)
        @assert 1 <= value <= N
        state = State(UInt8(value))
        rule = TransitionRule(symbol, direction, state)
        successor = step!(set_rule(tm, rule))
        if can_halt(successor)
            push!(result, successor)
        end
        states &= states - one(states)
    end
    return result
end

function push_successors!(
    result::Vector{TuringMachine{N}}, tm::TuringMachine{N}
) where {N}
    if !has_halted(tm)
        if has_transition(tm)
            push!(result, step!(tm))
        else
            if all_states_defined(tm.transition_table, tm.state[])
                push!(result, step!(set_rule(tm, HALT_RULE)))
            end
            states = distinct_states(tm.transition_table, tm.state[])
            push_successors!(result, tm, TapeSymbol(false), LEFT, states)
            push_successors!(result, tm, TapeSymbol(false), RIGHT, states)
            push_successors!(result, tm, TapeSymbol(true), LEFT, states)
            push_successors!(result, tm, TapeSymbol(true), RIGHT, states)
        end
    end
    return result
end

################################################################################

function TuringMachine{N}(indices::Vector{Int}) where {N}
    @assert N > 1
    @assert length(indices) >= 1
    @assert (indices[1] == 0) || (indices[1] == 1)
    result = TuringMachine(set_rule(
        TransitionTable{N}(),
        DEFAULT_SYMBOL,
        DEFAULT_STATE,
        TransitionRule(
            TapeSymbol(!iszero(indices[1])),
            RIGHT,
            State(DEFAULT_STATE.value + one(DEFAULT_STATE.value))
        )
    ))
    for i = 2:length(indices)
        while true
            while (!has_halted(result)) && has_transition(result)
                step!(result)
            end
            @assert !has_halted(result)
            successors = Vector{TuringMachine{N}}()
            push_successors!(successors, result)
            if length(successors) > 1
                if has_halted(successors[1])
                    @assert 1 <= indices[i] + 1 <= length(successors)
                    result = successors[indices[i]+1]
                else
                    @assert 1 <= indices[i] <= length(successors)
                    result = successors[indices[i]]
                end
                break
            else
                @assert !isempty(successors)
                result = successors[1]
            end
        end
    end
    table = result.transition_table
    if (!has_rule(table, HALT_RULE)) && (count_rule(table, NULL_RULE) == 1)
        return TuringMachine{N}(
            replace_rule(table, NULL_RULE, HALT_RULE),
            result.state,
            result.position,
            result.tape
        )
    else
        return result
    end
end

function TuringMachine{N}(tag::AbstractString) where {N}
    return TuringMachine{N}([parse(Int, part) for part in split(tag, '.')])
end

################################################################################

export tape_window, in_cycle

function tape_window(tm::TuringMachine{N}, n::Int) where {N}
    return BitSet(
        i - tm.position[]
        for i in tm.tape
        if abs(i - tm.position[]) <= n
    )
end

function in_cycle(
    table::TransitionTable{N}, state::State, window::BitSet, n::Int
) where {N}
    tm = TuringMachine{N}(table, fill(state), fill(0), copy(window))
    for _ = 1:n
        if has_halted(tm) || !has_transition(tm)
            return false
        end
        step!(tm)
    end
    return (state == tm.state[]) && (window == tape_window(tm, n))
end

################################################################################

end # module Busybeavers
