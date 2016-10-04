# Madison_UHI_data_processing

#1. BACKGROUND
Since 2012, we have maintained a network of ~150 temperature and humidity sensors around Dane County.  The sensors are located on the north side of light and utility poles at a height of ~3.5 m, and they record measurements every 15 minutes.  The sensors fill their memories after ~221 days, which requires downloading data twice per year, typically in April and October.  This requires driving to each location in a cargo van and climbing up the poles to retrieve sensors and download data.

The sensors are swapped between locations after each download, which speeds the process and helps us monitor sensor calibration issues (serial numbers are embedded in downloaded data, so we can track the sensors; there are some duplicate serial numbers, but the movement of each sensor is typically obvious based on where they are located).  Batteries are changed once per year (spring).

#2. DATA PROCESSING
These scripts process the data from the annual spring and fall downloads.  The scripts should be run in order.  They will merge all individual files together, merge with past years, and perform various meteorological conversions (e.g. Celsius to F, VPD, GDDs, HDDs, daily means, etc.).

The serial number script allows serial numbers to be extracted from files to track the location of individual sensors, which allows us to test for defective or poorly calibrated sensors.  To use this script, you must export the .hobo files with the serial number option selected.
