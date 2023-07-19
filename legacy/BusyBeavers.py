from __future__ import annotations as _annotations
from collections import defaultdict as _defaultdict
from enum import Enum as _Enum
from typing import NamedTuple as _NamedTuple
from typing import Iterator as _Iterator


################################################################################


class State(int):
    def to_string(self) -> str:
        if self == HALT:
            return "H"
        else:
            return chr(ord("A") + self)

    @staticmethod
    def from_string(state: str) -> State:
        if state == "H":
            return HALT
        else:
            return State(ord(state) - ord("A"))


HALT: State = State(-1)
DEFAULT_STATE: State = State(0)


################################################################################


class TapeSymbol(int):
    def to_string(self) -> str:
        return str(self)

    @staticmethod
    def from_string(symbol: str) -> TapeSymbol:
        return TapeSymbol(int(symbol))


TAPE_ALPHABET: list[TapeSymbol] = [TapeSymbol(0), TapeSymbol(1)]
DEFAULT_SYMBOL: TapeSymbol = TAPE_ALPHABET[0]


################################################################################


Tape = _defaultdict[int, TapeSymbol]


################################################################################


class TapeDirection(_Enum):
    LEFT = -1
    RIGHT = +1

    def to_string(self) -> str:
        if self == TapeDirection.LEFT:
            return "L"
        elif self == TapeDirection.RIGHT:
            return "R"
        else:
            raise ValueError("received unknown TapeDirection")

    @staticmethod
    def from_string(direction: str) -> TapeDirection:
        if direction == "L":
            return TapeDirection.LEFT
        elif direction == "R":
            return TapeDirection.RIGHT
        else:
            raise ValueError("received invalid TapeDirection string")


################################################################################


class TransitionKey(_NamedTuple):
    symbol: TapeSymbol
    state: State


class TransitionRule(_NamedTuple):
    symbol: TapeSymbol
    direction: TapeDirection
    state: State

    def to_string(self) -> str:
        return (
            self.symbol.to_string()
            + self.direction.to_string()
            + self.state.to_string()
        )

    @staticmethod
    def from_string(rule: str) -> TransitionRule:
        if len(rule) != 3:
            raise ValueError("received rule string of invalid length")
        return TransitionRule(
            TapeSymbol.from_string(rule[0]),
            TapeDirection.from_string(rule[1]),
            State.from_string(rule[2]),
        )


################################################################################


TransitionRow = tuple[TransitionRule | None, ...]


class TransitionTable(object):
    num_states: int
    data: dict[TransitionKey, TransitionRule]

    def __init__(self, num_states: int) -> None:
        assert num_states >= 0
        self.num_states = num_states
        self.data = {}

    def __len__(self) -> int:
        return len(self.data)

    def __contains__(self, key: TransitionKey) -> bool:
        assert key.symbol in TAPE_ALPHABET
        assert 0 <= key.state < self.num_states
        return key in self.data

    def __getitem__(self, key: TransitionKey) -> TransitionRule:
        assert key.symbol in TAPE_ALPHABET
        assert 0 <= key.state < self.num_states
        return self.data[key]

    def __setitem__(self, key: TransitionKey, rule: TransitionRule) -> None:
        assert key.symbol in TAPE_ALPHABET
        assert 0 <= key.state < self.num_states
        assert rule.symbol in TAPE_ALPHABET
        assert HALT <= rule.state < self.num_states
        self.data[key] = rule

    def copy(self) -> TransitionTable:
        result = TransitionTable(self.num_states)
        for key, rule in self.data.items():
            result[key] = rule
        return result

    def states(self) -> _Iterator[State]:
        for i in range(self.num_states):
            yield State(i)

    def defined_states(self) -> set[State]:
        return {key.state for key, _ in self.data.items()}

    def get_row(self, state: State) -> TransitionRow:
        assert 0 <= state < self.num_states
        return tuple(
            self.data.get(TransitionKey(symbol, state), None)
            for symbol in TAPE_ALPHABET
        )

    def distinct_states(
        self, special_state: State | None = None
    ) -> _Iterator[State]:
        if special_state is not None:
            assert 0 <= special_state < self.num_states
        seen_rows: set[TransitionRow] = set()
        for state in self.states():
            if state == special_state:
                yield state
            else:
                row = self.get_row(state)
                if row not in seen_rows:
                    seen_rows.add(row)
                    yield state

    def can_halt_from(self, state: State) -> bool:
        assert 0 <= state < self.num_states
        state_stack: list[State] = [state]
        seen_states: set[State] = set()
        while state_stack:
            state = state_stack.pop()
            if state not in seen_states:
                seen_states.add(state)
                for symbol in TAPE_ALPHABET:
                    key = TransitionKey(symbol, state)
                    if key not in self:
                        return True
                    rule = self[key]
                    if rule.state == HALT:
                        return True
                    state_stack.append(rule.state)
        return False

    def to_string(self) -> str:
        result: list[str] = []
        for state in self.states():
            for symbol in TAPE_ALPHABET:
                key = TransitionKey(symbol, state)
                if key in self:
                    result.append(self[key].to_string())
                else:
                    result.append("???")
        return "".join(result)

    @staticmethod
    def from_string(table: str) -> TransitionTable:
        ROW_SIZE = 3 * len(TAPE_ALPHABET)
        assert len(table) % ROW_SIZE == 0
        num_states = len(table) // ROW_SIZE
        result = TransitionTable(num_states)
        entries = [table[3 * i : 3 * i + 3] for i in range(len(table) // 3)]
        for i, entry in enumerate(entries):
            if entry != "???":
                key = TransitionKey(
                    TapeSymbol(i % len(TAPE_ALPHABET)),
                    State(i // len(TAPE_ALPHABET)),
                )
                rule = TransitionRule.from_string(entry)
                result[key] = rule
        return result


################################################################################


class TuringMachine(object):
    transition_table: TransitionTable
    state: State
    tape: Tape
    position: int

    def __init__(
        self,
        num_states: int,
        transition_table: TransitionTable | None = None,
        initial_state: State = DEFAULT_STATE,
        initial_tape: Tape | None = None,
        initial_position: int = 0,
    ) -> None:
        assert num_states > 1
        if transition_table is None:
            self.transition_table = TransitionTable(num_states)
        else:
            assert num_states == transition_table.num_states
            self.transition_table = transition_table
        self.state = initial_state
        if initial_tape is None:
            self.tape = _defaultdict(lambda: DEFAULT_SYMBOL)
        else:
            self.tape = initial_tape
        self.position = initial_position

    @staticmethod
    def from_string(table: str) -> TuringMachine:
        transition_table = TransitionTable.from_string(table)
        return TuringMachine(transition_table.num_states, transition_table)

    def copy(self) -> TuringMachine:
        return TuringMachine(
            self.transition_table.num_states,
            self.transition_table.copy(),
            self.state,
            self.tape.copy(),
            self.position,
        )

    def has_halted(self) -> bool:
        return self.state == HALT

    def get_current_key(self) -> TransitionKey:
        assert not self.has_halted()
        return TransitionKey(self.tape[self.position], self.state)

    def has_defined_step(self) -> bool:
        return (not self.has_halted()) and (
            self.get_current_key() in self.transition_table
        )

    def step(self) -> None:
        assert self.has_defined_step()
        rule = self.transition_table[self.get_current_key()]
        self.tape[self.position] = rule.symbol
        self.position += rule.direction.value
        self.state = rule.state
        return None

    def at_left_tape_edge(self) -> bool:
        return all(self.position < index for index in self.tape)

    def at_right_tape_edge(self) -> bool:
        return all(self.position > index for index in self.tape)

    def nondeterministic_step(self) -> list[TuringMachine]:
        assert not self.has_halted()
        if self.has_defined_step():
            successor = self.copy()
            successor.step()
            return [successor]
        else:
            states: list[State] = []
            new_rules: list[TransitionRule] = []

            # Only consider transitioning to the halt state if
            # all other states have at least one transition defined.
            defined = self.transition_table.defined_states()
            halt_eligible = all(
                (state == self.state) or (state in defined)
                for state in self.transition_table.states()
            )

            # If all other entries of the transition table are filled,
            # then we only consider transitioning to the halt state.
            table_size = self.transition_table.num_states * len(TAPE_ALPHABET)
            if len(self.transition_table) + 1 < table_size:
                states.extend(
                    self.transition_table.distinct_states(self.state)
                )

            # When transitioning to the halt state, the
            # tape symbol and direction do not matter.
            if halt_eligible:
                new_rules.append(
                    TransitionRule(TAPE_ALPHABET[1], TapeDirection.RIGHT, HALT)
                )

            for symbol in TAPE_ALPHABET:
                for direction in TapeDirection:
                    for state in states:
                        new_rules.append(
                            TransitionRule(symbol, direction, state)
                        )

            key = self.get_current_key()
            result: list[TuringMachine] = []
            for rule in new_rules:
                successor = self.copy()
                successor.transition_table[key] = rule
                if successor.transition_table.can_halt_from(successor.state):
                    successor.step()
                    result.append(successor)
            return result


################################################################################


def _main():
    from sys import argv

    TuringMachineRecord = tuple[tuple[int, ...], TuringMachine, int]

    def initial_machines(
        num_states: int,
    ) -> list[TuringMachineRecord]:
        result: list[TuringMachineRecord] = []
        for i in range(2):
            machine = TuringMachine(num_states)
            key = TransitionKey(DEFAULT_SYMBOL, DEFAULT_STATE)
            rule = TransitionRule(
                TAPE_ALPHABET[i], TapeDirection.RIGHT, State(DEFAULT_STATE + 1)
            )
            machine.transition_table[key] = rule
            result.append(((i,), machine, 0))
        return result

    def all_successors(records: list[TuringMachineRecord]):
        result: list[TuringMachineRecord] = []
        for tag, machine, num_steps in records:
            if machine.has_halted():
                result.append((tag, machine, num_steps))
            else:
                if machine.has_defined_step():
                    machine.step()
                    result.append((tag, machine, num_steps + 1))
                else:
                    successors = machine.nondeterministic_step()
                    if len(successors) > 1:
                        for index, successor in enumerate(successors):
                            result.append(
                                (tag + (index,), successor, num_steps + 1)
                            )
                    else:
                        for index, successor in enumerate(successors):
                            result.append((tag, successor, num_steps + 1))
        return result

    num_states = int(argv[1])
    machines = initial_machines(num_states)
    i = 0
    while True:
        print(
            "Writing {0} Turing machines to file after {1} steps.".format(
                len(machines), i
            )
        )
        with open("BB{0}-{1:08}.txt".format(num_states, i), "w+") as f:
            for tag, machine, num_steps in machines:
                f.write(
                    "{0} {1} {2} {3}\n".format(
                        ".".join(str(t) for t in tag),
                        machine.transition_table.to_string(),
                        "H" if machine.has_halted() else "R",
                        num_steps,
                    )
                )
        print("Computing step {0}.".format(i + 1))
        machines = all_successors(machines)
        i += 1


if __name__ == "__main__":
    _main()
