using Base.Threads

push!(LOAD_PATH, @__DIR__)
using BusyBeavers

const NUM_THREADS = nthreads()

function thread_view(v::Vector{T}, i::Int) where {T}
    base_size, num_big = divrem(length(v), NUM_THREADS)
    lower = base_size * (i - 1) + min(i, num_big + 1)
    upper = base_size * i + min(i, num_big)
    return view(v, lower:upper)
end

function main(::Val{N}, tag::Vector{Int}, n::Int) where {N}
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
        local_results = [
            Vector{Tuple{Vector{Int},TuringMachine{N},Int}}()
            for j = 1:NUM_THREADS
        ]
        @threads for j = 1:NUM_THREADS
            local_view = thread_view(labeled_machines, j)
            result = local_results[j]
            println("Thread $j working on $(length(local_view)) machines.")
            for (label, tm, num_steps) in local_view
                if has_halted(tm)
                    push!(result, (label, tm, num_steps))
                else
                    successors = Vector{TuringMachine{N}}()
                    push_successors!(successors, tm)
                    if length(successors) > 1
                        if has_halted(successors[1])
                            for (index, successor) in enumerate(successors)
                                push!(result, (
                                    push!(copy(label), index - 1),
                                    successor,
                                    num_steps + 1
                                ))
                            end
                        else
                            for (index, successor) in enumerate(successors)
                                push!(result, (
                                    push!(copy(label), index),
                                    successor,
                                    num_steps + 1
                                ))
                            end
                        end
                    else
                        for (_, successor) in enumerate(successors)
                            push!(result, (
                                label,
                                successor,
                                num_steps + 1
                            ))
                        end
                    end
                end
            end
            println("Thread $j done.")
        end
        labeled_machines = Vector{Tuple{Vector{Int},TuringMachine{N},Int}}()
        for local_result in local_results
            append!(labeled_machines, local_result)
        end
    end
end

main(
    Val{parse(Int, ARGS[1])}(),
    [parse(Int, piece) for piece in split(ARGS[2], '.')],
    parse(Int, ARGS[3])
)
