# The Indo-Pacific Pollen Database - a graphical overview

Developed by Ondrej Mottl

# Reference

This code is currently supporting the publication "The Indo–Pacific Pollen Database – a Neotoma constituent database" ([DOI: 10.5194/cp-20-2473-2024](https://doi.org/10.5194/cp-20-2473-2024).

## General info

It is a recommended structure for R projects which consists of modular codes with individual functions and purposes.
The **Config file** is the single file where all variables, packages, criteria are defined. Operations are singled out in folders and R-codes.

## Setup

* `___Init_project___.R` set up project on each machine. **EACH USER SHOULD RUN THIS SCRIPT FIRST**
* `00_Config_file` Config file is the master file in terms of setting all criteria used throughout the repo, loading the required packages and saving all settings throughout the repo.
