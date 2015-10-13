#!/bin/sh
MONO_GC_PARAMS=nursery-size=16m mono refinements/bin/Debug/negation.exe $*
