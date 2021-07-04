#!/bin/bash
gmt begin ddpnsum1over2_error pdf png A0.5c
  gmt set FONT_TAG 15p FONT_HEADING 20p MAP_HEADING_OFFSET 0p
  gmt gmtset MAP_FRAME_PEN  0.03c
  gmt gmtset MAP_TICK_LENGTH -0.1c
  gmt gmtset MAP_TICK_PEN_PRIMARY 0.01c
  gmt gmtset FONT_ANNOT_PRIMARY 10p
  gmt gmtset FONT_LABEL 12p
  gmt gmtset FONT_TITLE 12p
  gmt gmtset MAP_ANNOT_OFFSET_PRIMARY 0.2c
  gmt gmtset  MAP_TITLE_OFFSET 5p

gmt subplot begin 5x4 -Fs4c/3c -M0.1c/0.1c

for (( ti=1; ti<=19; ti++ )); do
  # 计算绘图id
  id=`let $ti-1`
  gmt subplot set $id
  # 输出补0的字符串给ii
  ii=`printf "%02d\n" $ti`
  pi="p"$ti

  if (( $ti < 17 ))
  then
    xl="Wsne"
  else
    xl="WSne"
  fi
  # title="P@-"$ti"@-"
  # title="@~\266@-\161@-@~P@-"$ti"@-"
  title="@~\266@-\161\161@-@~P@-"$ti"@-"

  file="2021-epsilon1over2-ddp"$ii"sum.txt"
  read -r y1 <<< $(sort -k5,5 -g $file | head -1 | awk '{print $5}')
  read -r y2 <<< $(sort -k5,5 -g $file | tail -1 | awk '{print $5}')
  # gmt basemap -R-1/1/$y1/$y2 -JX4c/3c -Bxa+L'cos@~(\161)@~' -BW$xl+t"P"@-$ti@- -Bya
  id=$(( $ti % 4 ))
  if (( $id==1 ));then
    gmt basemap -R0/180/$y1/$y2 -JX4c/3c -Bxa+L'@~\161(\260)@~' -BW$xl+t$title -Bya+l"Log(RE)"
  else
    gmt basemap -R0/180/$y1/$y2 -JX4c/3c -Bxa+L'@~\161(\260)@~' -BW$xl+t$title -Bya
  fi

  awk '{print $1,$5}' $file | gmt plot -Sc2p -W0.2p,blue -Gwhite
  awk '{print $1,$5}' $file | gmt plot       -W0.5p,blue
done

gmt subplot end
gmt end


