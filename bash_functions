#!/bin/bash

wsldir() {
    find $* -type d -print | xargs chmod 0755
}
