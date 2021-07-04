%% this program will generate the table 1
clear
c = 1/sqrt(2);
epsilon = 1/8;
sums(19,4) = 0;
for ii= 1:19
    sums(ii,1) = ii;
    sums(ii,2) =     pnm_sum(ii,epsilon,c);
    sums(ii,3) =   dpnm_sum(ii,epsilon,c);
    sums(ii,4) = ddpnm_sum(ii,epsilon,c);
end
fid = fopen('table_of_sums_octave.txt','w');
fprintf(fid,'%s\n','# c = cos(theta) = 1/sqrt(2), epsilon = 1/8');
fprintf(fid,'%s\n','# ID     Pnm              dPnm             ddPnm');
fprintf(fid,'%02d %16.4e %16.4e %16.4E\n',sums');
fclose(fid);