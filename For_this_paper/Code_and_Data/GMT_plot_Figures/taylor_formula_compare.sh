gmt begin taylor pdf png A0.5c
  gmt set FONT_TAG 15p FONT_HEADING 20p MAP_HEADING_OFFSET 0p
  gmt gmtset MAP_FRAME_PEN  0.03c
  gmt gmtset MAP_TICK_LENGTH -0.1c
  gmt gmtset MAP_TICK_PEN_PRIMARY 0.01c
  gmt gmtset FONT_ANNOT_PRIMARY 10p
  gmt gmtset FONT_LABEL 12p
  gmt gmtset FONT_TITLE 12p
  gmt gmtset MAP_ANNOT_OFFSET_PRIMARY 0.2c

gmt subplot begin 5x4 -Fs4c/3c -M0.3c/-0.2c -A'(1)'
  color1=black
  color2=red
  color3=blue
  color4=green
  color5=orange
  color6=purple


for (( ti=1; ti<=19; ti++ )) # 循环规则
do #开始循环

# 绘图
  # 计算绘图id
  id=`let $ti-1`
  gmt subplot set $id
  # 输出补0的字符串给ii
  ii=`printf "%02d\n" $ti`
  pi="P"$ti

  if (( $ti < 17 ))
  then
  xl="Wsne"
  else
  xl="WSne"
  fi

# gmt basemap -R1e-4/1e-1/0.98/1.02 -JX4cl/3c -Bxa1pf3+L'sin@~(\161)@~' -Bya+L$pi -BW$xl
  gmt basemap -R1e-4/1e-1/0.99/1.01 -JX4cl/3c -Bxa1pf3+L'sin@~(\161)@~' -Bya -BW$xl
  awk '{print $1,$2}' "p"$ii"sum_at_theta0_twomethods.txt" | gmt plot -W1.0p,$color1
  awk '{print $1,$3}' "p"$ii"sum_at_theta0_twomethods.txt" | gmt plot -W1.0p,$color2
  awk '{print $1,$4}' "p"$ii"sum_at_theta0_twomethods.txt" | gmt plot -W1.0p,$color3
#
done #循环结束

  gmt legend -DjBR+w2c+o-2.3c/0c+l1.5 << EOF
S 0.4c - 0.5c - 2p,$color1 1c Involving P@-i@- when @~\145@~=1
S 0.4c - 0.5c - 2p,$color2 1c Involving @~\266@-\161@-@~P@-i@- when @~\145@~=1
S 0.4c - 0.5c - 2p,$color3 1c Involving @~\266@-\161\161@-@~P@-i@- when @~\145@~=1
EOF

gmt subplot end
gmt end show
