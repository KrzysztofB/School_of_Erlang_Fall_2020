class: middle, center

# Processes in Erlang

School of Erlang

---
# Erlang processes

 - lightweight (grow and shrink dynamically)

 - easy to [garbage collect](http://erlang.org/doc/apps/erts/GarbageCollection.html)

 - small memory footprint

 - fast to create and terminate

 - the [scheduling](https://hamidreza-s.github.io/erlang/scheduling/real-tiis/preemptive/migration/2016/02/09/erlang-scheduler-details.html) overhead is low



---
# Data handling

Data in Erlang is immutable.

When one process passes data to the other one, data is copied between them.

Each process has its memory and its stack.

Several [optimizations](https://en.wikipedia.org/wiki/Persistent_data_structure) are applied inside BEAM.

Be careful about [big binaries and possible memory leaks](https://dieswaytoofast.blogspot.com/2012/12/erlang-binaries-and-garbage-collection.html).

---
# How to start a new process?

Use one of following functions:

 - [spawn](http://erlang.org/doc/man/erlang.html#spawn-1)

 - [spawn_link](http://erlang.org/doc/man/erlang.html#spawn_link-1)

 - [spawn_monitor](http://erlang.org/doc/man/erlang.html#spawn_monitor-1)

---
# How to start a new process? - Example

**Live demo**

This example show how the new process is started.

```
erl
% compile example file
c(example1).

% run
example1:main().
```

---
# Monitors

When process _A_ monitors process _B_ and _B_ terminates with exit reason `Reason`, a `'DOWN'` message is sent to _A_:

```
{'DOWN', Ref, process, B, Reason}
```

Process _A_ is **NOT** terminated.

[Read more in the docs](http://erlang.org/doc/reference_manual/processes.html#monitors)

---
# Monitors demo

```
erl
% compile example file
c(example_monitor).

% run
example_monitor:main().
```

---
# Links

Two processes can be linked to each other.
A link between two processes _A_ and _B_ is created by _A_ calling the BIF `link(B)` (or conversely).
There also exist a number of `spawn_link` BIFs, which spawn and link to a process in one operation.
Links are bidirectional and there can only be one link between two processes.
Repeated calls to `link(Pid)` have no effect.

[Read more in the docs](http://erlang.org/doc/reference_manual/processes.html#links)

---
# Links - Visual Explanation

.center[ .scale450x450[![Typing disciplines diagram](https://learnyousomeerlang.com/static/img/link-exit.png)] ]

[taken from](https://learnyousomeerlang.com/errors-and-processes)

---
# Links - Linked but did not die...

When two processes are linked, and one process calls `process_flag(trap_exit, true).` and the other process dies,
even though the first process is linked to the second one it is not terminated.
It is not recommended to use this flag usually, but its good to know it exists.

Sometimes useful to detect bugs [read more](https://github.com/aleklisi/cowboy_termiante_exit).

---
# Processes communication

Processes communicate by sending and receiving messages.
Message sending is **asynchronous** and safe.
The message is guaranteed to eventually reach the recipient, _provided that the recipient exists_.

---
# Sending messages

Using operator `!`:

`Destination ! Msg`

where `Destination` is a PID or name of a process.

You can also use [erlang:send/2,3](http://erlang.org/doc/man/erlang.html#send-2) or
[erlang:send_after/3,4](http://erlang.org/doc/man/erlang.html#send_after-3).

The message will [eventually](https://youtu.be/IP-rGJKSZ3s) be delivered.

[Read more in the docs](http://erlang.org/doc/reference_manual/expressions.html#send)

---
# Receiving messages

Each process has a   message queue.
If you do not match on a specific pattern it works as FIFO.

```
receive
    Pattern1 ->
        Body1;
    Pattern2 [when GuardSeq2] ->
        Body2;
    ...;
    PatternN [when GuardSeqN] ->
        BodyN
end
```

---
# Sending and receiving messages - Example

This example show how Message is send and received.

**Live demo**

```
erl
% compile example file
c(example2).

% run
example2:main().
```

---
# Selective message picking - Example

You can determine an order of messages received by matching to them.

**Live demo**

```
erl
% compile example file
c(example3).

% run
example3:main().
```

---
# How to access process?

- via PID as variable
- [register/2](http://erlang.org/doc/man/erlang.html#register-2)



---
# Behaviors

Behaviors are formalizations of these common patterns.
The idea is to divide the code for a process in a generic part
(a behavior module) and a specific part (a callback module).

They are very much like interfaces in OOP.

Examples are:

- [gen_event](https://erlang.org/doc/man/gen_event.html)

- [gen_statem](https://erlang.org/doc/man/gen_statem.html)

- [gen_server](http://erlang.org/doc/design_principles/gen_server_concepts.html)

[Read more in the docs](http://erlang.org/doc/design_principles/des_princ.html#behaviours)

---
# gen_server

 - `init` initialization
 - `handle_cast` asynchronous call
 - `handle_call` synchronous call
 - `handle_info` messages handling

[gen_server example](http://erlang.org/doc/design_principles/gen_server_concepts.html#example)
[gen_server docs](http://erlang.org/doc/man/gen_server.html)

---
# Read more:

 - [Processes](http://erlang.org/doc/efficiency_guide/processes.html)

 - [Concurrent Programming](http://erlang.org/doc/getting_started/conc_prog.html)

 - [Intro to Concurrency](https://learnyousomeerlang.com/the-hitchhikers-guide-to-concurrency)
