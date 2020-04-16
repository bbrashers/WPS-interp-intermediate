# WPS-interp-intermediate
Interpolate in time (not in space) between two WPS Intermediate version 5 files

This program reads two files in WPS Intermediate version 5 format, 
interpolates the gridded values in time, and writes an output file.

WPS Intermediate format can be read by METGRID.EXE, which is why these
files are sometimes called 'metgrid' files.

Type "interp-intermediate -h" for a helpful message about use.

The most important detail is if, for some reason, the two files you
are interplating between have different missing_value values.  This
value is NOT in the header of the files, which is really too bad
(would have been a good design decision).  You need to know the
missing_values ahead of time.

If specifying all three missing_values, give the output one first
("-m val") because it also sets the missing_value for the two input
files.  This is so you can set the value for all three in one step.
If you then also give the "-m1 val1" and/or the "-m2 val2" after the
"-m val" it will use val1 and val2 correctly.

In the ZIP file, the files SNOWH:2011-01-01_12 and SNOWH:2011-01-02_12
were created from SNODAS data, using my snodas2metgrid program.  The
file SNOWH:2011-01-02_00 was interpolated between them.

Hint: use the un-documented -p I,J flag to print the values before and
after, and the interpolated value, at the point I,J.

Bart Brashers
bbrashers@ramboll.com
2020-04-16
