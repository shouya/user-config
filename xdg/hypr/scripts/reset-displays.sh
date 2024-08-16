#!/bin/bash

hyprctl keyword monitor DP-5,disable
hyprctl keyword monitor DP-5,prefered,auto,2
hyprctl dispatch dpms on
