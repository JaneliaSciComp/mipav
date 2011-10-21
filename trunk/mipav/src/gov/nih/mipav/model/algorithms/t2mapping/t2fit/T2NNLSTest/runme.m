fp = fopen('set009_ls.txt', 'rt');
nte = fscanf(fp,'%f\n', 1);
A = fscanf(fp, '%f %f %f\n', nte*3);
te = A(1:3:end);
de = A(2:3:end);
e = A(3:3:end);
nt2 = fscanf(fp,'%f\n', 1);
A = fscanf(fp, '%f %f\n', nt2*2);
x = A(1:2:end);
t2s = A(2:2:end);
chi2 = fscanf(fp,'%f\n', 1);
fclose(fp);


frac_low = 1.02;
frac_high = 1.025;

%-----------------------------------------------------------------
%
%  Compute the LS solution.
%
fprintf(1, 'Solving ls solution ...');

A = exp(-kron(te, 1./t2s'));
x = lsqnonneg( A ./ repmat(e, [1, length(t2s)]), de ./ e, zeros(length(t2s),1),[]);
yy = A * x;
chi2 = sum( (yy - de).^2 ./ (e).^2 );

[x(x>0) t2s(x>0)]


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
	
tic
while( chi2 < frac_low*chi2_min | chi2 > frac_high*chi2_min )
	fprintf('-----------------------\n');
	fprintf('%f %f %f\n', chi2, frac_low*chi2_min, frac_high*chi2_min);
	fprintf('mu=%f\n', mu);

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

		%Ah(length(te)+[1:length(t2s)],[1:length(t2s)]) = mu*ones(length(t2s),1);
		Ah(indices) = mu;
	end
end
toc

fprintf(1, 'chi2 = %4.4f (mu=%g)\n', chi2,mu);
