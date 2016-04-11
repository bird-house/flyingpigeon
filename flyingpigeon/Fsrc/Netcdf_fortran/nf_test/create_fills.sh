#!/bin/sh
# This shell script which creates the fill.nc file from fill.cdl.
# $Id: create_fills.sh,v 1.1 2016/03/25 16:39:10 sradanov Exp $

echo
echo "*** Testing creating file with fill values."
set -e
#../ncgen/ncgen -b $srcdir/fills.cdl
cp ${TOPSRCDIR}/nf_test/ref_fills.nc ./fills.nc
echo "*** SUCCESS!"
exit 0
