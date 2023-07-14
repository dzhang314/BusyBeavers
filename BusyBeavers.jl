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

export State, DEFAULT_STATE, HALT

struct State
    value::UInt8
end

const DEFAULT_STATE = State(0x01)
const HALT = State(0x00)

################################################################################

export TransitionRule, get_symbol, get_direction, get_state, NULL_RULE

struct TransitionRule
    data::UInt8
end

function TransitionRule(
    symbol::TapeSymbol, direction::TapeDirection, state::State
)
    data = UInt8(0x00)
    if symbol.value
        data |= 0x80
    end
    if direction.value
        data |= 0x40
    end
    data |= state.value
    return TransitionRule(data)
end

function get_symbol(rule::TransitionRule)
    return TapeSymbol((rule.data & 0x80) != 0x00)
end

function get_direction(rule::TransitionRule)
    return TapeDirection((rule.data & 0x40) != 0x00)
end

function get_state(rule::TransitionRule)
    return State(rule.data & 0x3F)
end

const NULL_RULE = TransitionRule(0xFF)

################################################################################

export TransitionTable

struct TransitionTable{N}
    data::NTuple{N,NTuple{2,TransitionRule}}
end

function TransitionTable{N}() where {N}
    return TransitionTable{N}(ntuple(_ -> ntuple(_ -> NULL_RULE, 2), N))
end

################################################################################

export TuringMachine, has_halted, has_transition, step!

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

function has_halted(tm::TuringMachine{N}) where {N}
    return tm.state[] == HALT
end

function get_rule(tm::TuringMachine{N}) where {N}
    table = tm.transition_table.data
    state = tm.state[].value
    @assert 1 <= state <= N
    row = @inbounds table[state]
    return (tm.position[] in tm.tape) ? row[2] : row[1]
end

function has_transition(tm::TuringMachine{N}) where {N}
    return get_rule(tm) != NULL_RULE
end

function step!(tm::TuringMachine{N}) where {N}
    @assert !has_halted(tm)
    @assert has_transition(tm)
    rule = get_rule(tm)
    Base._setint!(tm.tape, tm.position[], get_symbol(rule).value)
    if get_direction(rule).value
        tm.position[] += 1
    else
        tm.position[] -= 1
    end
    tm.state[] = get_state(rule)
    return tm
end

################################################################################

end # module Busybeavers
