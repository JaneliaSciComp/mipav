frac_low = 1.02;
frac_high = 1.025;

%-----------------------------------------------------------------
%
%  Get the filename to write to.
%
ii = 1;
while( exist(sprintf('set%03d_ls.txt', ii), 'file') == 2 )
	ii = ii + 1;
end

filename = sprintf('set%03d_ls.txt', ii);

%-----------------------------------------------------------------
%
%  Setup the variables.
%
t2s = [logspace(log10(10), log10(2000), 80)  999999999];
te = [10:10:320]';
d = 1000*(0.2*exp(-te/15) + 0.8*exp(-te/80));
e1 = normrnd(0, 10, 32, 1);
e2 = normrnd(0, 10, 32, 1);
de = sqrt( (d + e1).^2 + e2.^2 );
e = sqrt( e1.^2 + e2.^2 );

%-----------------------------------------------------------------
%
%  Compute the LS solution.
%
fprintf(1, 'Solving ls solution ...');

A = exp(-kron(te, 1./t2s));
x = lsqnonneg( A ./ repmat(e, [1, length(t2s)]), de ./ e, zeros(length(t2s),1),[]);
yy = A * x;
chi2 = sum( (yy - de).^2 ./ (e).^2 );

fprintf(1, 'chi2 = %4.4f\n', chi2);

%-----------------------------------------------------------------
%
%  Write out ls solution.
%
filename = sprintf('set%03d_ls.txt', ii);
fp=fopen(filename, 'wt+');
fprintf(fp, '%d\n', length(te));
fprintf(fp, '%8.8f %8.8f %8.8f\n',  [te, de, e]');
fprintf(fp, '%d\n', length(x));
fprintf(fp, '%8.8f %8.8f\n', [x t2s']');
fprintf(fp, '%8.8f\n', chi2);
fclose(fp);

%-----------------------------------------------------------------
%
%  Compute the small solution.
%
fprintf(1, 'Solving small solution ...');

mu = -0.01;

Ah = [ A./ repmat(e, [1, length(t2s)]) ; mu*eye(length(t2s)) ];
deh = [de ./ e; zeros(length(t2s),1)];

indices = find( Ah == mu );

mu = 0.01;
Ah(indices) = mu;

chi2_min = chi2;
	
while( chi2 < frac_low*chi2_min | chi2 > frac_high*chi2_min )
	%
	%  Calculate the new solution.
	%
	x = lsqnonneg(Ah, deh, zeros(length(t2s),1),[]);
	yy = A * x;
	chi2 = sum( (yy - de).^2 ./ (e).^2 );

	if( chi2 < frac_low*chi2_min | chi2 > frac_high*chi2_min )
		%
		%  Update mu.
		%
		if( chi2 < frac_low*chi2_min )
			mu = mu*1.5;
		end

		if( chi2 > frac_high*chi2_min )
			mu = mu/1.4;
		end

		Ah(indices) = mu;
	end
end

fprintf(1, 'chi2 = %4.4f\n', chi2);

%-----------------------------------------------------------------
%
%  Write out small solution.
%
filename = sprintf('set%03d_small.txt', ii);
fp=fopen(filename, 'wt+');
fprintf(fp, '%d\n', length(te));
fprintf(fp, '%8.8f %8.8f %8.8f\n',  [te, de, e]');
fprintf(fp, '%d\n', length(x));
fprintf(fp, '%8.8f %8.8f\n', [x t2s']');
fprintf(fp, '%8.8f\n', chi2);
fclose(fp);
