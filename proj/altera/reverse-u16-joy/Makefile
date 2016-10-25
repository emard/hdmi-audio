###################################################################
# Project Configuration: 
# 
# Specify the name of the design (project) and the Quartus II
# Settings File (.qsf)
###################################################################

PROJECT = project
TOP_LEVEL_ENTITY = hdmiaudio_reverseu16
ASSIGNMENT_FILES = $(PROJECT).qpf $(PROJECT).qsf

###################################################################
# Part, Family, Boardfile
FAMILY = "Cyclone IV E"
PART = EP4CE22E22C8
BOARDFILE = reverse-u16.board
CONFIG_DEVICE = EPCS16
SERIAL_FLASH_LOADER_DEVICE = EP4CE22
OPENOCD_BOARD=reverse-u16.ocd
# OPENOCD_INTERFACE=interface/altera-usb-blaster.cfg
# OPENOCD_INTERFACE=ftdi-fpu1.ocd
OPENOCD_INTERFACE=remote.ocd

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
top/hdmiaudio_reverseu16.vhd \
clocks/pll_50M_250M_25M_83M333.vhd \
clocks/clk_50M_125M_25M.vhd \
../../../modules/video-generator/vga.vhd \
../../../modules/osd/osd.vhd \
../../../modules/osd/char_rom.v \
../../../modules/hdmi-audio/av_hdmi.vhd \
../../../modules/hdmi-audio/hdmidataencoder.v \
../../../modules/hdmi-audio/encoder.vhd \
../../../modules/hdmi-audio/altera/cyclone4/serializer.vhd \
../../../modules/hdmi-audio/hdmidelay.vhd \
../../../modules/hdmi-audio/serializer_generic.vhd \
../../../modules/vnc2hid/uart_deserializer.vhd \
../../../modules/vnc2hid/vnc2hid.vhd
