#!/bin/bash -e

SAMPLE=jumps

/home/tilo/code/nasm/nasm -o samples/${SAMPLE}.bin samples/${SAMPLE}.asm
ndisasm samples/${SAMPLE}.bin > ${SAMPLE}.lst
cargo run -p mrc-asm -- samples/${SAMPLE}.asm
ndisasm out.com > out.lst
vimdiff ${SAMPLE}.lst out.lst

