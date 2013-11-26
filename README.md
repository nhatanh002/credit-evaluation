credit-evaluation
=================

A credit evaluation expert system written in R5RS Scheme

==================
INSTALLATION GUIDE
==================
Generally, first you should install the Chicken implementation of the Revised^5 Report
of the Scheme language. Then you install the required readline ``egg'' for Chicken.
Readline is simply a package for I/O, and has nothing to do with the 
knowledge-engineering aspect of the system.

# Linux

For Debian/Ubuntu users:
;installing Chicken
user@localhost $ sudo apt-get install chicken-bin libchicken-dev libchicken6
;installing readline
user@localhost $ sudo chicken-install readline

For Fedora/Redhat users:
$ sudo yum install chicken
$ sudo chicken-install readline

and so on...

For other distributions, just search for the name of the Chicken package in your
distro's repository, and use your respective package manager to install. You can also 
build Chicken from source yourself, and install readline egg normally like the above 
examples.

# BSDs

For FreeBSD, you can use the lang/chicken port to install the latest stable release.

For NetBSD, you can use the lang/chicken package from pkgsrc to install the latest stable release.

For OpenBSD, you can use the lang/chicken package by running the following command as root:
$ pkg_add chicken

For DragonFly BSD, you can use the lang/chicken package from pkgsrc to install the latest stable release.

# MacOSX

If you're using MacPorts, installation is very simple:
$sudo port install chicken
$port install readline 
$chicken-install readline

More information at http://wiki.call-cc.org/platforms#mac-os-x

# Microsoft Windows
Using MS Windows is not recommended.
Installer for Windows: http://www.kiatoa.com/cgi-bin/chicken-iup/home

==============================
USING CREDIT EVALUATION SYSTEM
==============================

Just load main.scm into Chicken and run!

$ csi main.scm

