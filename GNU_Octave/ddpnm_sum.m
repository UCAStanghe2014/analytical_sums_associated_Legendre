function p = ddpnm_sum(id, t, c)
w = sqrt(1+t.^2-2.*t.*c);
s = sqrt(1-c.^2);
p4 =  9.*s.^2.*t.*(1+15.*c.*t+18.*((-2)+c.^2).*t.^2+c.*((-39)+c.^2).*t.^3+(-3).*((-20)+c.^2).*t.^4+(-9).*c.*t.^5+(-8).*t.^6).*w.^(-11)+((-4).*c.^4.*t.^3+9.*c.^3.*t.^2.*((-6)+t.^2)+(-3).*t.*((-5)+13.*t.^2+3.*t.^4)+3.*c.^2.*t.*((-10)+27.*t.^2+6.*t.^4)+c.*((-1)+72.*t.^2+(-66).*t.^4+8.*t.^6)).*w.^(-9);
p3 =  35.*s.^2.*t.^5.*w.^(-9)+(-1).*c.*w.^(-5)+(-5).*c.*t.^4.*w.^(-9).*(7.*s.^2+w.^2)+(-5).*t.^3.*w.^(-9).*((-7)+7.*c.^4+21.*s.^2+(-1).*w.^2+3.*s.^2.*w.^2)+t.*w.^(-7).*((-5)+15.*s.^2+(-2).*w.^2+4.*s.^2.*w.^2)+c.*t.^2.*w.^(-9).*(35.*s.^2+5.*w.^2+25.*s.^2.*w.^2+w.^4);
p2 =  w.^(-7).*((-1).*c.*w.^2+c.*t.^2.*(5.*s.^2+2.*w.^2)+(-1).*t.*(w.^2+s.^2.*((-5)+10.*t.^2+(-2).*w.^2)));
p1 =  w.^(-5).*(3.*s.^2.*t+(-1).*c.*w.^2);
p5 =  (-1/4).*(8.*c+4.*((-7)+4.*s.^2).*t+c.*(35+c.^2+(-3).*s.^2).*t.^2+4.*((-5)+3.*s.^2).*t.^3+4.*c.*t.^4).*w.^(-3).*(1+(-1).*c.*t+w).^(-2)+(t.*(5+(-2).*s.^2+t.^2)+(-2).*c.*(1+2.*t.^2)).*w.^(-2).*(1+(-1).*c.*t+w).^(-2);
p6 =  2.*s.^2.*t.*w.^(-2).*(1+(-1).*c.*t+w).^(-2)+((-1).*c+t+2.*s.^2.*t).*w.^(-1).*(1+(-1).*c.*t+w).^(-2)+(1+(-1).*c.*t+w).^(-2).*((-6).*c.^2.*t+c.*w+t.*(4+((-3)+5.*s.^2).*w))+t.*w.^(-3).*(1+(-1).*c.*t+w).^(-2).*(s.^2+c.*t.*((-1)+c.^2.*(1+(-1).*w.^3.*((-3)+log(2)))))+c.*(1+(-1).*c.*t+w).^(-2).*((-1).*t.^2.*log(2)+(-1).*(1+w).*log(4)+c.*t.*((-2)+w.*log(4)+log(16)))+c.*(1+(-1).*c.*t+w).^(-2).*((-1).*((-2)+s.^2).*t.^2+2.*(1+w)+(-2).*c.*t.*(2+w)).*log(1+(-1).*c.*t+w);
p7 =  t.^(-2).*((-1).*t+c.*t.^2).*w.^(-3)+s.^(-2).*t.^(-2).*w.^(-1).*((-1).*c+t+c.*w);
p8 =  (-5).*s.*t.^3.*((-98).*c.^4.*t+16.*c.^3.*(7+2.*t.^2)+6.*c.^2.*t.*(11+3.*t.^2)+t.*(79+63.*s.^4+(-45).*t.^2+2.*t.^4)+c.*((-139)+(-34).*t.^2+7.*t.^4)).*w.^(-11)+s.*w.^(-5).*((-2)+w.^2)+c.*s.*t.*w.^(-7).*((-50)+9.*w.^2)+5.*s.*t.^2.*w.^(-9).*(28+w.^4+(-1).*c.^2.*(49+6.*w.^2));
p9 =  s.*w.^(-9).*(35.*s.^2.*t.^2.*(1+(c+(-2).*t).*t)+5.*t.*(2.*t+c.*((-3)+(-5).*c.*t+6.*t.^2)).*w.^2+((-1)+2.*t.*((-2).*c+t)).*w.^4);
p10 =  15.*s.^3.*t.^2.*w.^(-7)+(-1).*s.*w.^(-5).*(9.*c.*t+w.^2);
p11 =  (-3).*s.*(c+(-1).*t).*t.*w.^(-5)+(-1).*s.*w.^(-3)+s.*t.^2.*w.^(-3).*(1+(-1).*c.*t+w).^(-2).*((-1).*c.*t+(1+w).^2);
p12 =  s.*((-3).*(1+(-1).*c.*t).*w.^(-5)+w.^(-3)+w.^(-2).*(1+(-1).*c.*t+w).^(-1).*(w.^(-1)+(1+w).^2.*(1+(-1).*c.*t+w).^(-1)));
p13 =  (-1).*s.*w.^(-3)+(-1).*s.^(-1).*w.^(-1)+s.^(-3).*t.^(-1).*((1+c.^2).*log((1+c).*(c+(-1).*t+w).^(-1))+2.*c.*log((1/2).*(1+(-1).*c.*t+w)));
p14 =  15.*c.*(2+(-19).*s.^2).*t.^2.*w.^(-7)+12.*(1+(-2).*s.^2).*t.*w.^(-5)+105.*s.^2.*t.^4.*w.^(-11).*(9.*c.*s.^2+(-9).*s.^2.*t+5.*c.*w.^2)+15.*t.^3.*w.^(-9).*((-35).*s.^2+63.*s.^4+2.*(1+(-2).*c.^2).*w.^2);
p15 =  3.*t.*w.^(-9).*(35.*s.^4.*t.^2+(-25).*c.*s.^2.*t.*w.^2+(2+(-4).*s.^2).*w.^4);
p16 =  3.*s.^2.*t.*((-3)+t.*(c+2.*t)).*w.^(-7)+3.*c.*w.^(-5).*((-1)+c.*t+w.^2)+2.*c.*s.^(-2).*w.^(-3).*(1+c.*t+2.*w.^2)+2.*s.^(-2).*t.^(-1).*w.^(-3).*((-2)+2.*w.^3)+6.*c.*s.^(-4).*t.^(-1).*w.^(-1).*(t+c.*((-2)+c.*t+2.*w));
p17 =  (9.*c+4.*c.^3+(-1).*c.^5).*s.^(-4)+5.*c.*t.^2.*((-11)+c.^2+10.*c.*t+(-30).*t.^2).*w.^(-7)+(-5).*s.^(-2).*t.*(3+c.^4+20.*c.^2.*t.^2.*(2+t.^2)+(-4).*c.*t.*(5+10.*t.^2+t.^4)).*w.^(-7)+(c+(-1).*t).*(1+2.*t.*(c+6.*t)).*w.^(-5)+(-2).*s.^(-2).*(c+(-1).*t).*((-1)+t.^2.*((-25)+6.*c.*t+(-4).*t.^2)).*w.^(-5)+12.*s.^(-4).*(c+(-1).*t).*((-1)+(-1).*t.^2.*(6+t.^2)+4.*c.*(t+t.^3)).*w.^(-5);
p18 =  (-7).*c.*s.^(-4).*(31+5.*c.^4+(-26).*s.^2).*w.^(-7)+14.*s.^(-4).*(5+c.^4+(-5).*s.^2).*t.^(-1).*w.^(-7)+s.^(-4).*(181+200.*c.^4+39.*c.^6+(-183).*s.^2).*t.*w.^(-7)+(-1).*c.*s.^(-4).*(3.*(99+40.*c.^4+c.^6)+(-241).*s.^2).*t.^2.*w.^(-7)+s.^(-4).*(103+155.*c.^4+(-6).*c.^6+(-86).*s.^2).*t.^3.*w.^(-7)+4.*s.^(-4).*((-3)+2.*s.^2).*(7.*c+(-1).*t).*t.^4.*w.^(-7)+2.*c.*(5+c.^2).*s.^(-4).*t.^(-2).*w.^(-7).*((-1)+w.^7);
p19 =  (-3).*s.^2.*t.*w.^(-5)+s.^(-2).*t.^(-1).*w.^(-1).*((-2)+(-1).*c.^2+(-3).*c.*t+2.*w+c.^2.*w)+(-1).*t.^(-1).*w.^(-3).*(t.^2+w.^2+(-1).*w.^3)+(-12).*c.^2.*s.^(-4).*t.^(-1).*log(2)+(-1).*s.^(-2).*t.^(-1).*log(16)+2.*c.*(5+c.^2).*s.^(-4).*t.^(-1).*log((-1).*((-1)+c).^(-1).*((-1).*c+t+w))+4.*(1+2.*c.^2).*s.^(-4).*t.^(-1).*log(1+(-1).*c.*t+w);
if 0<id && id<20
    p = eval(strcat('p',int2str(id)));
end
end