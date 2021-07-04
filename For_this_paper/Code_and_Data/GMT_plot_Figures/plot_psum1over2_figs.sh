#!/bin/bash
gmt begin pnsum1over2 pdf png A0.5c
  gmt set FONT_TAG 15p FONT_HEADING 20p MAP_HEADING_OFFSET 0p
  gmt gmtset MAP_FRAME_PEN  0.03c
  gmt gmtset MAP_TICK_LENGTH -0.1c
  gmt gmtset MAP_TICK_PEN_PRIMARY 0.01c
  gmt gmtset FONT_ANNOT_PRIMARY 10p
  gmt gmtset FONT_LABEL 12p
  gmt gmtset FONT_TITLE 12p
  gmt gmtset MAP_ANNOT_OFFSET_PRIMARY 0.2c
  gmt gmtset MAP_TITLE_OFFSET 5p

gmt subplot begin 5x4 -Fs4c/3c -M0.1c/0.1c

for (( ti=1; ti<=19; ti++ )) # 循环规则
  do #开始循环
  # 绘图
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
    file="2021-epsilon1over2-p"$ii"sum.txt"
    title="P@-"$ti"@-"
    # title="@~\266@-\161@-@~P@-"$ti"@-"
    # title="@~\266@-\161\161@-@~P@-"$ti"@-"


    read -r y1 y2 <<< $(awk '(NR==1){Min=$3;Max=$3};(NR>=2){if(Min>$3) Min=$3;if(Max<$3) Max=$3} END {printf "%f %f",Min,Max}' $file)
    gmt basemap -R0/180/$y1/$y2 -JX4c/3c -Bxa+L'@~\161(\260)@~' -BW$xl+t$title -Bya

    if (($ti<19))
    then
      awk '{print $1,$3}' $file | gmt plot -Sc2p -W1.0p,grey
      awk '{print $1,$4}' $file | gmt plot -W1.0p,blue
    else
      awk '{print $1,$3}' $file | gmt plot -Sc2p -W1.0p,grey  -l'Numerical'
      awk '{print $1,$4}' $file | gmt plot -W1.0p,blue        -l'Analytical'
    fi
  #
done #循环结束

  gmt legend -DjBR+o-4.5c/0.1c -F

# S 0.4c - 0.5c - 2p,$color1 1c Involving P@-i@- when @~\145@~=1
# S 0.4c - 0.5c - 2p,$color2 1c Involving @~\266@-\161@-@~P@-i@- when @~\145@~=1
# S 0.4c - 0.5c - 2p,$color3 1c Involving @~\266@-\161\161@-@~P@-i@- when @~\145@~=1
# EOF

gmt subplot end
gmt end


