#!/usr/bin/env bash

PAKET_EXE=.paket/paket.exe
FAKE_EXE=packages/build/FAKE/tools/FAKE.exe

mono $PAKET_EXE restore
mono $FAKE_EXE build.fsx Docker