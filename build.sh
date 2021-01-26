#!/bin/bash

nasm -felf64 tmp.asm -o tmp.o
ld -o tmp tmp.o
chmod u+x tmp