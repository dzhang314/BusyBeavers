push!(LOAD_PATH, @__DIR__)
using BusyBeavers

function main(::Val{N}, tag::Vector{Int}, n::Int) where {N}
    @assert N > 1
    labeled_machines = [(
        tag,
        TuringMachine{N}(TuringMachine{N}(tag).transition_table),
        0
    )]
    for i = 0:n
        println(
            "Writing ", length(labeled_machines),
            " Turing machines to file after $i steps."
        )
        filename = "BB$N-$(join(tag, '.'))-$(string(i; base=10, pad=8)).txt"
        open(filename, "w+") do io
            for (label, tm, num_steps) in labeled_machines
                println(
                    io,
                    join(label, '.'),
                    " ",
                    to_string(tm.transition_table),
                    " ",
                    has_halted(tm) ? "H" : "R",
                    " ",
                    num_steps
                )
            end
        end
        println("Computing step ", i + 1, ".")
        labeled_successors = Vector{Tuple{Vector{Int},TuringMachine{N},Int}}()
        for (label, tm, num_steps) in labeled_machines
            if has_halted(tm)
                push!(labeled_successors, (label, tm, num_steps))
            else
                successors = Vector{TuringMachine{N}}()
                push_successors!(successors, tm)
                if length(successors) > 1
                    if has_halted(successors[1])
                        for (index, successor) in enumerate(successors)
                            push!(labeled_successors, (
                                push!(copy(label), index - 1),
                                successor,
                                num_steps + 1
                            ))
                        end
                    else
                        for (index, successor) in enumerate(successors)
                            push!(labeled_successors, (
                                push!(copy(label), index),
                                successor,
                                num_steps + 1
                            ))
                        end
                    end
                else
                    for (_, successor) in enumerate(successors)
                        push!(labeled_successors, (
                            label,
                            successor,
                            num_steps + 1
                        ))
                    end
                end
            end
        end
        labeled_machines = labeled_successors
    end
end

main(
    Val{parse(Int, ARGS[1])}(),
    [parse(Int, piece) for piece in split(ARGS[2], '.')],
    parse(Int, ARGS[3])
)
