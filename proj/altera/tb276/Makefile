###################################################################
# Project Configuration: 
# 
# Specify the name of the design (project) and the Quartus II
# Settings File (.qsf)
###################################################################

PROJECT = project
TOP_LEVEL_ENTITY = hdmiaudio_tb276
ASSIGNMENT_FILES = $(PROJECT).qpf $(PROJECT).qsf

###################################################################
# Part, Family, Boardfile
FAMILY = "Cyclone IV E"
PART = EP4CE6E22C8
BOARDFILE = tb276.board
CONFIG_DEVICE = EPCS4
SERIAL_FLASH_LOADER_DEVICE = EP4CE6
OPENOCD_INTERFACE=interface/altera-usb-blaster.cfg
# OPENOCD_INTERFACE=ftdi-fpu1.ocd
# OPENOCD_INTERFACE=remote.ocd

###################################################################
#
# Quartus shell environment vars
#
###################################################################

quartus_env ?= . ../include/quartus_env.sh

# include makefile which does it all
include ../include/altera.mk

###################################################################
# Setup your sources here
SRCS = \
top/hdmiaudio_tb276.vhd \
clocks/clk_25M_125M_25M.vhd \
clocks/pll_25M_250M_25M.vhd \
../../../modules/video-generator/vga.vhd \
../../../modules/hdmi-audio/av_hdmi.vhd \
../../../modules/hdmi-audio/hdmidataencoder.v \
../../../modules/hdmi-audio/encoder.vhd \
../../../modules/hdmi-audio/altera/cyclone4/serializer.vhd \
../../../modules/hdmi-audio/hdmidelay.vhd \
../../../modules/hdmi-audio/serializer_generic.vhd \
