push!(LOAD_PATH, @__DIR__)
using BusyBeavers


struct MachineRecord{N}
    tag::Vector{Int}
    machine::TuringMachine{N}
    num_steps::Int
end


function compute_successors(records::Vector{MachineRecord{N}}) where {N}
    result = Vector{MachineRecord{N}}()
    for record in records
        if has_halted(record.machine)
            push!(result, record)
        else
            successors = Vector{TuringMachine{N}}()
            push_successors!(successors, record.machine)
            if length(successors) > 1
                if has_halted(successors[1])
                    for (index, successor) in enumerate(successors)
                        push!(result, MachineRecord{N}(
                            push!(copy(record.tag), index - 1),
                            successor,
                            record.num_steps + 1
                        ))
                    end
                else
                    for (index, successor) in enumerate(successors)
                        push!(result, MachineRecord{N}(
                            push!(copy(record.tag), index),
                            successor,
                            record.num_steps + 1
                        ))
                    end
                end
            else
                for (_, successor) in enumerate(successors)
                    push!(result, MachineRecord{N}(
                        record.tag,
                        successor,
                        record.num_steps + 1
                    ))
                end
            end
        end
    end
    return result
end


function write_machines_to_file(
    tag::Vector{Int},
    records::Vector{MachineRecord{N}},
    chunk_size::Int, current_step::Int, final_step::Int
) where {N}
    if (length(records) > 1) && (current_step % 100 == 0)
        println("Computing step $current_step for $(length(records)) Turing machines at tag $(join(tag, '.')).")
    end
    if current_step >= final_step
        println("Writing $(length(records)) Turing machines to file at tag $(join(tag, '.')) after $current_step steps.")
        filename = "BB$N-$(join(tag, '.'))-$(string(current_step; base=10, pad=8)).txt"
        open(filename, "w+") do io
            for record in records
                println(io,
                    join(record.tag, '.'),
                    ' ',
                    to_string(record.machine.transition_table),
                    ' ',
                    has_halted(record.machine) ? 'H' : 'R',
                    ' ',
                    record.num_steps
                )
            end
        end
        return nothing
    else
        successors = compute_successors(records)
        if length(successors) <= chunk_size
            return write_machines_to_file(tag, successors, chunk_size, current_step + 1, final_step)
        else
            tag_range = 1:length(tag)+1
            tag_prefix = view(successors[1].tag, tag_range)
            chunk = [successors[1]]
            for i = 2:length(successors)
                current_prefix = view(successors[i].tag, tag_range)
                if current_prefix == tag_prefix
                    push!(chunk, successors[i])
                else
                    write_machines_to_file(collect(tag_prefix), chunk, chunk_size, current_step + 1, final_step)
                    tag_prefix = current_prefix
                    chunk = [successors[i]]
                end
            end
            write_machines_to_file(collect(tag_prefix), chunk, chunk_size, current_step + 1, final_step)
            return nothing
        end
    end
end


function main(::Val{N}, tag::Vector{Int}, n::Int) where {N}
    write_machines_to_file(
        tag,
        [MachineRecord{N}(
            tag,
            TuringMachine{N}(TuringMachine{N}(tag).transition_table),
            0
        )],
        99999,
        0,
        n
    )
    return nothing
end


main(
    Val{parse(Int, ARGS[1])}(),
    [parse(Int, piece) for piece in split(ARGS[2], '.')],
    parse(Int, ARGS[3])
)
