#!/bin/bash

nasm -felf64 tmp.asm
ld -o tmp tmp.o
chmod u+x tmp