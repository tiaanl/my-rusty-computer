#!/usr/bin/env bash

nasm -f bin -o TEST.COM test.asm

${DOSBOX} -conf sample.cfg



