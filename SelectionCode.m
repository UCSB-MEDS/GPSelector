clear all
close all

fmat = xlsread('GP2017a','Point Allocations'); %should be Nxk matrix where N is number of students and k is num of projects
B = xlsread('GP2017a','Project Maximums'); %should be kx1

k = size(B,1); %The number of projects
N = size(fmat,1);
if size(fmat,2) ~= k
    disp('Matrices are not entered correctly')
end

f = -reshape(fmat',N*k,1);
Beq = ones(N,1);
Aeq1 = [ones(1,k) zeros(1,N*k)];
Aeq2 = repmat(Aeq1,1,N);
Aeq3 = Aeq2(1:N*N*k); 
Aeq = reshape(Aeq3,N*k,N)';

A0 = [1 zeros(1,k-1)];
A1 = [repmat(A0,1,N) 0];
A2 = repmat(A1,1,k);
A3 = A2(1:N*k*k);
A = reshape(A3,N*k,k)';

numVars = N*k;
intcon = 1:numVars;
lb = zeros(numVars,1);
ub = ones(numVars,1);

[X Fval Exit] = intlinprog(f,intcon,A,B,Aeq,Beq,lb,ub);
Solution = reshape(X,k,N)'
-Fval
Exit

xlswrite('GP2017a.xlsx',Solution,'Solution','D3')
