---
title: Understanding the GNU Mach API
date: March 31, 2017
tags: programming, system
...

# Introduction

[GNU Mach](https://www.gnu.org/software/hurd/microkernel/mach.html") is the
microkernel developed for the Hurd system. It is a free (as in free speech)
working microkernel. Despite its power, it is not very used and thus the
documentation is lacking. This article aims to cover the principles of the mach
kernel, and some of its API, to ease the development of any mach based system.

In order to test the code examples, you will need a working installation of a
GNU mach system. The main one is the GNU Hurd operating system. You can refer to
my [previous article](/articles/2017-02-28-hacking-hurd-workflow.html) for an
installation guide. Since the code we will be writing is very low level, it will
be architecture dependant. Our target will be the x86 32-bits architecture,
which is what you will have if you used my previous article to install Hurd.

# Main concepts

## Execution units

A microkernel job is to provide an interface to the hardware. That means
abstracting the CPU and handling the execution of programs on it. Mach uses two
abstractions for this : tasks and threads. Execution units are threads : they
are used to store an execution, ie the value of registers. Their execution order
is decided by Mach, which handles the interleaving of their executions, and
splitting over multiple cores, transparently.

There is another important device the kernel must provide an interface to, and
that is memory. The solution used is that of virtual memory : each execution
unit believes it has the whole memory for itself, and thus let the kernel handle
where its memory really is, if it is swapped... But giving a whole memory map
for every thread would be inefficient, as we might want to have few executions
units sharing the same memory. The abstraction used for that is task : a task is
a virtual memory map and a set of threads. A thread must always be part of a
task, and has full access to the virtual memory of the task.

Tasks also provides some more convenience utilities. For example, killing or
pausing a task kills/pause all of its threads. A few more utilities are provided
to handle errors. We will go back to that later. The equivalent of tasks in a
Linux system are processus (but these are more complexes : Mach's tasks have no
notion of users or permissions).

## Interprocess communication (IPC)

Now that we've seen how mach abstracts execution of programs, we need to look at
the other essential aspects of a microkernel : the IPC. In other words, how to
make tasks communicate. The abstraction is the port. A port is a queue of typed
messages. It is manipulated through ports rights. A port right is an integer
linked with a task that allows to enqueue and/or dequeue messages from the port
(a bit like a file descriptor in Linux).

So that means that the knowledge of the port right is the ability to use it.  It
is made so that another task cannot guess port rights it does not have.

Ports rights are task specific. This means that all the threads of a task share
the same port rights. When sending a message, a port right can be sent.  This
allow a ports to be created in a task, and then sent to other tasks.

They are three king of port rights : send, send-once and receive. The send right
is the ability to enqueue messages on the port. It can be copied to other tasks.
The send-once right is the ability to enqueue only one message on the port. The
receive right is the ability to dequeue messages from the port. Only one task
can hold the receive right for a given port. It can be used to create send and
send-once rights for the port, and can be transfered between tasks.

Messages are dequeued in the order they are enqueued. Enqueueing a message will
block if the port is full, and dequeueing will if it is empty. Both these
operations can use a timeout. If multiple threads try to receive from a port at
the same time, every message will be received only once, but which thread will
receive which message is undefined.

Ports right can be grouped into ports sets. This allows to wait a message from
multiple ports, reading from the first one to be ready (it is slightly similar
to the `select` system call in Linux).

## Controlling execution

It is possible to pause, resume and kill any thread independently, or whole
tasks at once. When a thread is paused, it is possible access its state (ie the
value of its registers) and change it (doing this in an unposed thread will
cause undefined behaviours). This implies a thread cannot access its own state.

Every thread and task also has an exception port. When an error is raised
(either manually, or because of a segfault/division by zero ...), a message is
sent to the thread exception port, or if it is null to the task exception port.
This allows for debugging/recovering from errors.

# Using the GNU mach API

The API documentation can be found
[here](https://www.gnu.org/software/hurd/gnumach-doc/index.html) , but since it
is quite terse, I will detail the ipc mechanism and the thread creation through
examples.

A bit of warning : it seems the documentation is not completely up to date when
it comes to return values. Indeed, after looking at the source code of Mach, it
turns out some functions have more return values than what is described in this
documentation. Just remember that all function returns a `kern_return_t`, which
is `KERN_SUCCESS` in case of success, and another value in case of error. I
won't dig into error handling other than the existence of error, so it won't
bother me in the next examples. Also, and despite the fact that I use `perror`,
Mach calls does not set `errno`.

## A minimal IPC example

Here I will detail how to create a port, send an integer to it and receive this
integer from, all from the same thread. This is quite a cheap example, but the
even basic IPC is not so trivial, so it may be interesting to focus on that
first. You can find the code
[there](https://github.com/dwarfmaster/mach-ipc/blob/master/minimal_ipc/main.c).
I will assume you're looking at the code when reading the following, so I will
not write the code again here.

First we need to create a port. As said before, a port is just an integer, but
Mach defines an alias `mach_port_t` for this. The call for the creation is
`mach_port_allocate`, documented
[here](https://www.gnu.org/software/hurd/gnumach-doc/Port-Creation.html) . It
expects three argument. The third one is used to return the created port.  The
first one is the current task : indeed, we said before ports are task specific,
and thus we allocate a port for a certain task. And there is call just for that
: `mach_task_self()`, which returns the current task.  Finally, we precise which
rights we want on the port. We will want to send and receive, but since we can
derive send rights from receive ones, we will create it with receive rights, so
we use the constant `MACH_PORT_RIGHT_RECEIVE`.

Now we want to create utility functions to send and receive integers from a
port. We will first focus on sending a message. Thus, what we want to create is
a `send_integer` function, which takes a port and an integer, and sends it on
the port.

But we cannot send just anything on a port : message needs to have a certain
formatting. It must starts with a `mach_msg_header_t`, then information about
its type (either short or long). We will use short information so we need a
`mach_msg_type_t`. And then is the content of the message, in our case an
integer. Instead of creating buffers with the right size and copying structure
into it, we can directly create our own structure with the right members.

When sending a message, it need to have both the right header and the right type
info. First we focus on the header. The header must have data about the transfer
itself, rather than the message specifically. It must have the size of the whole
message (with the header and type information) in its `msgh_size` field. The
destination port is in `msgh_remote_port`.  A message can also specify another
port in `msgh_local_port`, which is often used as a reply port. In our case, we
don't need it, so we set it to `MACH_PORT_NULL`. Finally, we need to precise
which right these ports have in the `msgh_bits` field. We need to use the macros
`MACH_MSGH_BITS_REMOTE` to say the given right are to apply to the remote port,
and we use `MACH_MSG_TYPE_MAKE_SEND` to tell it to create send right from the
receive one (remember, we created the port with receive rights, because it is
possible to create send one from these), and to use them to enqueue the message.

Then on to the type header. This header is used by receiver to determine to kind
of data sent. Sending and receiving the message would still work without it, but
it is necessary when sending more complex data (like port rights, or out of line
data), so it is a good idea to learn how to set it. Most of the field are pretty
straightforward, like `msgt_size` or `msgt_number`. `msgt_name` is just a
classifier of data, mostly unnecessary, unless you send port rights to another
task, so that Mach can update them according to the new task. `msgt_inline`
tells whether the data is inside the message, or if their only is a pointer to
the data in the message. This field is particularly important when you
communicate with another task, which has another memory space, as Mach will
interpret it and copy the data to the other task memory space, and update the
pointer transparently. `msgt_deallocate`, in the case of an out of line, tells
Mach to free the data from the sender memory space. Finally, `msgt_longform`
specify whether the type header is the long one or none.

Now that we've got our well formatted header, we can proceed to send it.  The
magic function doing everything (which we will also use for reception) is the
`mach_msg` procedure. It's first argument is the message itself (it says it
expects a pointer to a header but since the message must start with a header it
is the same to give any of the pointer). The second argument precise the kind of
transfer. The third one is the size we want to send (here we set it to the size
of the message). The forth the size we want to receive (since we're only sending
a message, we set it to 0). Then the port on which to receive (we're not
receiving, so we set it to `MACH_PORT_NULL`). The timeout in case the queue is
full (we won't use that here). And finally, the last argument can be used for
certain modes of communication, which I didn't fully understand, so we'll not
use that and set it to `MACH_PORT_NULL`.

Here we are, you can now almost fully understand the `send_integer` procedure.

The `receive_integer` one is much more easier, since the message will be fully
set by the reception. Well, this is not completely true : `mach_msg` will use
the `msgh_size` of the header to determine how much size there is in the message
to fill. Please not this field may be changed according to the amount of data
received.

We can now easily use these functions on our newly created port to send and
receive our favorite integer !

## Adding threads

The code for this project is
[there](https://github.com/dwarfmaster/mach-ipc/blob/master/minimal_threads/main.c).
I kept the messaging utility we've seen in the previous section to test our new
thread. This code is not completely functional : for some reason, any hurd
system call like `printf` or `scanf` fails in the new thread, so we're limited
to Mach IPC in it.

Here is the standard procedure to create a working thread in Hurd : we create
the thread, which is paused without state initially. We then set the state and
resume the thread. Creating the thread is pretty straightforward with the
`thread_create` function, and resuming it is as easy with the `thread_resume`
function. Setting the state is where the difficulty lie.

Indeed, a thread state is the value of its registers. This is where the code
we're writing becomes architecture specific. Thankfully, Mach has utility to
describe the register values. First of all, there is the
`i386_THREAD_STATE_COUNT` which holds the number of registers to set, and the
`struct i386_thread_state` structure which allows us to easily set the value of
the registers.

Now let's focus on which registers we want to set. First of all, there is the
`eip` register which hold the pointer to the next instruction.  This one is easy
: since the new thread share the same memory space as the old one, we simply
need to set it to the pointer to the function we want to execute. We then need
to set the stack `uesp` and frame `ebp` pointer. It is sufficient to set `ebp`
to zero.  But to set `uesp` a question arise : where to we create our stack, and
how.

To create the stack, we simply allocate a big enough block of memory. Let's use
this occasion to see some functionality of the virtual memory of Mach).  I
decided (more or less arbitrarily : it copies what Hurd cthread library does) to
set the stack size to 16 pages. To find the page size, Mach defines a global
variable `mach_page_size`, which is of 4Kb in my 32-bit Qemu Hurd installation.
The memory is allocated with `vm_allocate`.  The first argument is the process
for which the memory must be allocated, the second one is used to return the
pointer to the newly allocated memory, the third one the size we want (which may
be rounded if it is not a multiple of a page size), and the forth one says if we
care about where it is allocated (FALSE value, in which case it will try to
allocated at the position specified by the second argument), or if it can
allocate it anywhere.

Once we've got our chunk of memory, there remain two things to do : set the
right values at the top so that the function see its argument, and protect to
bottom of the stack to detect stack overflows. This second thing is not
necessary but it allows us to use the virtual memory protection mechanism in
Mach. Since permissions can only be set on whole pages, we will protect the
bottom page of the stack from both reading and writing. The function to do so is
`vm_protect`. The first argument the process whose memory must be protected, the
second the address of the memory, and the third the size (which may be rounded
to a multiple of page size). According to my understanding, you can either set
the permissions or the maximum permission allowed, and this is controlled by the
forth argument, set to FALSE.

Now that we're protected from stack overflows, we need to setup the stack to be
able to call the function in the thread. In 32bits, all arguments are passed on
the stack. The stack pointer of the function must point to the element in the
stack holding the return address, and above it the arguments, the first one the
closest to return value. More information on
[wikipedia](https://en.wikipedia.org/wiki/Calling_convention#x86).  Once the
stack is ready, we can set `uesp`, and resume the thread.

We now have an utility function to start a thread. But what happen when the
thread ends ? We set the return value to 0, so of course it will segfault, but
there is no good position to return to, we simply want the thread to end. Well
Mach provides the procedure `thread_terminate`. The easiest way to circumvent
the problem is to call it on `mach_thread_self()` at the end of the routine. For
those of you who feel this solution is not satisfying because intrusive, feel
free to create a wrapper function to launch, taking in argument the pointer to
the routine.

Now our system to start thread is complete. Awfully complicated, isn't it.
Well, Hurd provide a library,
[cthreads](http://hurdextras.nongnu.org/ipc_guide/mach_ipc_cthreads.html),
allowing an easy use of threads on Mach. But that wouldn't have been fun,
wouldn't it ?

## Sieve of Eratosthenes as a Kahn thread network

We have seen, through minimal examples, how to use ipc and thread in mach.  Here
I will propose a small exercise to put it all together.

[Kahn networks](https://en.wikipedia.org/wiki/Kahn_process_networks) are a
theoretical model of calculus : a set of deterministic units able to
communicate. Here every unit will be a thread, communicating through ports.  The
[Sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes) is
an algorithm to find prime numbers by filtrating out the multiples of every
prime already found.

The idea of our implementation is the following : one thread writes into a port
every integer starting from 2. The main thread, every time it receives an
integer, print it, and add between itself and the previous thread a filter
communicating only the integers received that are not multiples of the prime
just found. At any moment, the number of threads in the task will be the number
of primes found more 2.

You now have all the cards you need to make it work. Have fun ! The solution is
[here](https://github.com/dwarfmaster/mach-ipc/blob/master/main.c).

# Conclusion

I hope to have given you enough knowledge to understand without to much trouble
the documentation of Mach. Still, there are a number of points left unexplained
:

 - How to catch exception (segfaults, division by zero ...) from the
   created threads.
 - How to handle hardware devices.
 - How to create a memory manager for Hurd.
 - Perhaps more importantly, how to use 
   [MIG](https://www.gnu.org/software/hurd/microkernel/mach/mig/gnu_mig.html),
   the Mach Interface Generator, to ease the creation of communication through
   ports.
 - How to use the Mach debugging features.
 - And probably a lot more.

