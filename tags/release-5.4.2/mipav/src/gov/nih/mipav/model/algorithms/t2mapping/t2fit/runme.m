te = 10:10:320;

stds = abs([ 7.1191 12.9025 6.6860 11.9084 -12.0246 -0.1979 -1.5672 -16.0409 2.5730 -10.5647 14.1514 -8.0509 5.2874 2.1932 -9.2190 -21.7067 -0.5919 -10.1063 6.1446 5.0774 16.9243 5.9128 -6.4360 3.8034 -10.0912 -0.1951 -0.4822 0.0004 -3.1786 10.9500 -18.7399 4.2818 ]);

ii = 1;
d_true(ii) = 827.3037; ii=ii+1;
d_true(ii) = 696.6165; ii=ii+1;
d_true(ii) = 594.4575; ii=ii+1;
d_true(ii) = 512.2916; ii=ii+1;
d_true(ii) = 444.6261; ii=ii+1;
d_true(ii) = 387.8507; ii=ii+1;
d_true(ii) = 339.5291; ii=ii+1;
d_true(ii) = 297.9667; ii=ii+1;
d_true(ii) = 261.9438; ii=ii+1;
d_true(ii) = 230.5514; ii=ii+1;
d_true(ii) = 203.0890; ii=ii+1;
d_true(ii) = 178.9999; ii=ii+1;
d_true(ii) = 157.8300; ii=ii+1;
d_true(ii) = 139.2015; ii=ii+1;
d_true(ii) = 122.7946; ii=ii+1;
d_true(ii) = 108.3353; ii=ii+1;
d_true(ii) = 95.5871; ii=ii+1;
d_true(ii) = 84.3441; ii=ii+1;
d_true(ii) = 74.4266; ii=ii+1;
d_true(ii) = 65.6771; ii=ii+1;
d_true(ii) = 57.9573; ii=ii+1;
d_true(ii) = 51.1456; ii=ii+1;
d_true(ii) = 45.1349; ii=ii+1;
d_true(ii) = 39.8309; ii=ii+1;
d_true(ii) = 35.1503; ii=ii+1;
d_true(ii) = 31.0198; ii=ii+1;
d_true(ii) = 27.3748; ii=ii+1;
d_true(ii) = 24.1581; ii=ii+1;
d_true(ii) = 21.3194; ii=ii+1;
d_true(ii) = 18.8143; ii=ii+1;
d_true(ii) = 16.6035; ii=ii+1;
d_true(ii) = 14.6525; ii=ii+1;

ii = 1;
de(ii) = 836.2794; ii=ii+1;
de(ii) = 703.9740; ii=ii+1;
de(ii) = 600.2419; ii=ii+1;
de(ii) = 512.8323; ii=ii+1;
de(ii) = 451.9340; ii=ii+1;
de(ii) = 393.6632; ii=ii+1;
de(ii) = 337.0125; ii=ii+1;
de(ii) = 294.2102; ii=ii+1;
de(ii) = 258.9955; ii=ii+1;
de(ii) = 215.8002; ii=ii+1;
de(ii) = 200.9999; ii=ii+1;
de(ii) = 180.4331; ii=ii+1;
de(ii) = 161.0217; ii=ii+1;
de(ii) = 154.0936; ii=ii+1;
de(ii) = 119.7513; ii=ii+1;
de(ii) = 115.5100; ii=ii+1;
de(ii) = 103.5791; ii=ii+1;
de(ii) = 94.5400; ii=ii+1;
de(ii) = 64.5070; ii=ii+1;
de(ii) = 68.7300; ii=ii+1;
de(ii) = 61.8264; ii=ii+1;
de(ii) = 41.1509; ii=ii+1;
de(ii) = 38.9011; ii=ii+1;
de(ii) = 50.6702; ii=ii+1;
de(ii) = 34.4662; ii=ii+1;
de(ii) = 36.8197; ii=ii+1;
de(ii) = 28.6276; ii=ii+1;
de(ii) = 17.9958; ii=ii+1;
de(ii) = 19.8597; ii=ii+1;
de(ii) = 26.7337; ii=ii+1;
de(ii) = 11.7128; ii=ii+1;
de(ii) = 22.4646; ii=ii+1;

frac_low = 1.02;
frac_high = 1.025;
alpha = 180;
t1 = 1000;
tr = inf;
t2s = [logspace(log10(15),log10(2000),80) 999999999];

%fprintf('Results with stds = 1\n');
%x = t2nnls(te, t2s, de, ones(1,32), frac_low, frac_high, t1, tr, alpha);
%[x(x>0) t2s(x>0)']

fprintf('Results with stds set properly\n');
x = t2nnls(te, t2s, de, 1.1*stds, frac_low, frac_high, t1, tr, alpha);
[x(x>0) t2s(x>0)']
