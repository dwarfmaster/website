---
title: "Hacking Hurd: setting up the workflow"
date: February 28, 2017
...

# Introduction

[Hurd](https://www.gnu.org/software/hurd/) is an interesting piece of software,
and digging in may be an interesting way of learning of the ins and outs of
operating systems. But you can't just download the source code, compile it and
run it as you would do with most of the programs.

Here I talk about setting up an environment for working on the _user space_ , ie
not the micro kernel. In this post I will cover how to install Hurd in a virtual
environment, how to use it, and how to set the whole thing up to ease the
writing and testing of software on Hurd.

# The installation

Hurd in itself is not an operating system, but only a kernel, and can't be used
on its own (like [Linux](http://www.tldp.org/LDP/sag/html/gnu-or-not.html)). So
what do we install. Well, like there are Linux distribution, there are Hurd
distributions, even though less numerous :

 - [Debian GNU/Hurd](http://www.debian.org/ports/hurd/)
   , a porting of the Debian GNU/Linux distribution over Hurd. It is the
   official distribution.

 - [ArchHurd](https://www.archhurd.org/)
   , a porting of the ArchLinux distribution.

 - [NixHurd](https://www.gnu.org/software/hurd/hurd/running/nix.html)
   , although it is more experimental and can only run in Qemu.

Since we are only interested in working on Hurd itself, I will propose to
install Debian GNU/Hurd, as it is the most stable of the three.

To ease the testing of the system, the installation will be done in a
[Qemu](http://www.qemu-project.org/) . To begin, you must install the `qemu`
package (or `qemu-kvm`) on your system and enable KVM in your Linux Kernel, to
speed up the simulation.

You must now setup Qemu. First of all, you must create a virtual disk on which
Hurd will installed. The command to do so is :

```shell
qemu-img create file.img size
```

where `size` is 3G for example (this capacity is indeed enough for what we will
do).

Once this is done, you can download the iso with the following command :

```shell
wget http://people.debian.org/~sthibault/hurd-i386/installer/cdimage/cd-1.iso
```

Once this is done, you can launch the system with :

```
qemu-kvm -m 1G -drive cache=writeback,file=file.img -cdrom cd-1.iso
```

The virtual machine will then boot the live CD, and you will get a grub where
you can choose the installation process. Go for the `Pseudo-graphical install`
which will give you a standard ncurses interface.

![GNU/Hurd Live CD](/images/hacking-hurd-workflow/qemu.png)

The installation process is well detailed : if you have ever installed a Linux
distribution, it shouldn't trouble you. Leaving the default option are fine in
most cases (the only trouble I got was that it failed when asked to install a
graphical environment, so I didn't install one).

Once this is finished, shutdown the virtual machine.

# Installing the software

For now, login as root to setup the environment. The user you created will be
used later.

First of all, we will need to upgrade the installed software, but simply running
`apt update` failed for me. The reason was that it first tried to update from
the cd. So the first step is to go edit the `/etc/apt/sources.list` file and
comment (or delete) the lines starting by `deb cdrom`.

Once you've done this, run : `apt update && apt upgrade`

You can now use `apt install` to install any software you might want. To turn
the virtual machine off, first use the `shutdown -h 0` command and close the
qemu window once you see the `running in tight loop` message.

## Sharing files with host

The workflow advocated by the [official
page](https://www.gnu.org/software/hurd/contributing.html#index4h2) is to
develop and compile directly in Hurd. The compiling part is necessary (unless
you manage to setup a cross compilation toolchain from Linux to Hurd), by if
like me you don't want to have to setup your whole programming environment in a
virtual machine, you may want to at least do the programming part on your host
system.

The workflow I will present is thus the following : you work on your host
system, send the files to the virtual machine, and compile and run there.  Since
Qemu does not support sharing a disk with the host, another solution must be
found. You could use git to synchronize the files, but it would mean committing
every change you want to test. Another solution would to setup a filesystem
sharing server on your host and connect to it from Hurd. But Hurd doesn't seem
to have an sshfs implementation and despite having a
[nfs](https://www.gnu.org/software/hurd/hurd/translator/nfs.html)
implementation, it seems to be [quite
experimental](https://www.gnu.org/software/hurd/community/gsoc/project_ideas/nfs.html)
.

We will use the fact that Hurd comes with a working ssh server to use rsync.
While qemu automatically setup networking so that the virtual environment has
access to internet and Hurd autoconnects, we will need to pass a flag to qemu so
that we can connect from the host to the guest. Remember that due to the
specific nature of ping, you cannot ping from the guest OS. Thus ping failing
does not mean you are not connected.

This flag is `-redir tcp:iph::ipg` : it will redirect any connection on
localhost port iph on the host to ipg port on the guest. For example, if you
launch qemu with the command :

```shell
qemu-kvm -m 1G -drive cache=writeback,file=file.img -redir tcp:2222::22<br>
```

you can ssh into your hurd installation with : `ssh -p 2222 root@localhost<br>`.
This may be an easier to work in hurd rather than in qemu.

Since qemu 2.6 complain about `-redir` begin deprecated, it may be a good idea
to replace it. The new option is `-net nic -net user,hostfwd=tcp::2222-:22` .

If you only want to work on you virtual machine through ssh, you may want to
launch qemu without a graphic interface (for example if you want to launch qemu
on a distant headless computer). To do that, append the `-nographic` option to
the previous command. If you want to keep the output on terminal, you can
replace `-nographic` by `-curses`.

In order for rsync to work, in must also be installed on the guest. Thus run :
`apt install rsync` in Qemu. You can then synchronize any directory with :

```shell
rsync -r --rsh='ssh -p 2222' dir user@localhost:/path/<br>
```

You will get a shitload of errors due to operations not being implemented in
Hurd's version of ext2, but the file will still be synchronized.

## Building the sources

All that is left is to install the right software in Hurd to build the
translator and run them. Since Hurd allows any translator to run in user space,
we will use the user created during the installation.

To build the source, you first need to install some tools and their
dependencies. To install the tools, you must run the following command :

```shell
apt install build-essential fakeroot git
```

And for the dependencies : `apt build-dep hurd`.

Then you must download the sources. They are managed with git, so all you have
to do is to clone their repo. The links are given on
[this](http://savannah.gnu.org/git/?group=hurd) page. Once you have all the
sources you want, you can copy them to Hurd using rsync as seen above.

To compile the code source, there are a few steps. First of all, you must
generate the configure script. To do so, run `autoconf` in the Hurd directory.
Then create a build directory, move into it and call the configure script from
it. Then all that's left is to run `make`.  You can pass to make the name of a
submodule to build only it.

# Conclusion

You now have a working environment to hack Hurd and contribute. Have fun !

