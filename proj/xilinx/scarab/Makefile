project = miniSpartan6-plus
vendor = xilinx
family = spartan6
# for different FPGA chip, parameter override:
# MODEL=xc6slx9 make
MODEL ?= xc6slx25
part = ${MODEL}-ftg256-3 # last number -3 is speed grade
flashsize = 8192 # KB (kilobytes) - SPI flash device is M25P40
top_module = hdmiaudio_scarab
isedir = /opt/Xilinx/ISE/14.7/ISE_DS
xil_env = . $(isedir)/settings64.sh
SHELL = /bin/bash
# openocd_interface = interface/altera-usb-blaster.cfg
# openocd_interface = ../include/ft2232-fpu1.ocd
openocd_interface = ../../include/ft2232-generic.ocd
xc3sprog_interface = ftdi

include ../include/xilinx.mk

vfiles = \
../../../modules/hdmi-audio/hdmidataencoder.v \

vhdfiles = \
top/hdmiaudio_scarab.vhd \
clocks/clk_50M_100M_125Mp_125Mn_25M.vhd \
../../../modules/video-generator/vga.vhd \
../../../modules/osd/osd.vhd \
../../../modules/osd/char_rom.vhd \
../../../modules/hdmi-audio/av_hdmi.vhd \
../../../modules/hdmi-audio/encoder.vhd \
../../../modules/hdmi-audio/hdmidelay.vhd \
../../../modules/hdmi-audio/serializer_generic.vhd \
../../../modules/hdmi-audio/xilinx/xc6/ddr_out.vhd \
../../../modules/hdmi-audio/xilinx/xc6/hdmi_out_xc6.vhd \
../../../modules/vnc2hid/uart_deserializer.vhd \
../../../modules/vnc2hid/vnc2hid.vhd

