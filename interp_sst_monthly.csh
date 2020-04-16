#!/bin/csh -f
if ($#argv == 0 | "$1" =~ -*h*) then
    echo "Usage: $0:t YYYY [begMO [endMO]]"
    echo ""
    echo "Runs interp-intermediate for the given month(s)."
    exit
endif

set yr = $1
set bmo = 1
set emo = 12
if ($#argv > 1) then
    set bmo = $2
    set emo = $bmo
    if ($#argv > 2) set emo = $3
endif

foreach param (SST)
    if !(-e $param) mkdir -p $param

    foreach mo (`seq -f "%02g" $bmo $emo`)
	set ndays = `daysinmth $yr $mo`
	foreach dy (`seq -f "%02g" 1 $ndays`)

	    set time1 = $yr-$mo-${dy}_12     # today
	    set time2 = `add_time $time1 24` # tomorrow

	    if !(-e $param/${param}:$time1) then
		echo "Starting file ${param}:$time1 does not exist"
	    else if !(-e $param/${param}:$time2) then
		echo "Ending   file ${param}:$time2 does not exist"
	    else
		foreach add (6 12 18) # add to get 18, 00, 06, already have 00.

		    set timeI = `add_time $time1 $add`
		    set out = $param/${param}:$timeI
		
		    if !(-e $out) then
			interp-intermediate -i \
			    $param/${param}:$time1    \
			    $param/${param}:$time2 \
			    -o $out
			echo Created $out
		    endif
		end
	    endif
	end
    end
end
