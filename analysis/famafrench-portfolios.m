clear all; % clear data from Octave
close all; % close all open plot windows
 
% Load Fama-French Data
ff_data = load('25_Portfolios_5x5_monthly_2.txt');
% Load FF Factor Mimicking Portfolios
ff_facts = load('F-F_Factors_monthly.txt');
 
% Starting point changed to January 1932 to avoid missing data
ff_data = ff_data(67:end,:);   % start after NAs end
ff_facts = ff_facts(67:end-1,:); % start after NAs end, factors had one extra sample, so used end-1
 
% Remove date column
r = ff_data(:,2:end);
% Remove date and risk free
ff3f = ff_facts(:,2:end);
 
% Prompt for User Input to get plotting range
startyear = input('Enter Starting Year between 1932 and 2010: ')
startmonth = input('Enter Starting Month 1-12: ')
endyear = input('Enter Ending Year between 1932 and 2010: ')
endmonth = input('Enter Ending Month 1-12: ')
plottitle = input('Enter Title for Plot: ','s')
 
% Calculate starting and ending row
start = 12*(startyear - 1932) + startmonth;
endpoint = 12*(endyear-1932) + endmonth;
 
% Extract Desired Data
r = r(start:endpoint,:);
ff3f = ff3f(start:endpoint,:);
rmrf = ff3f(:,1)/100;
smb = ff3f(:,2)/100;
hml = ff3f(:,3)/100;
rf = ff3f(:,4)/100;
 
% Run 25 Fama-French Regressions
rx = r./100 - repmat(rf,1,25);
 
% Run FF regressions on all portfolios
K = 3
T = size(rx,1)
X = [ones(T,1) rmrf hml smb];
b = X\rx;
e = rx-X*b;
sigma = cov(e);
u = rx-X*b;
s2 = (T-1)/(T-K-1)*var(u)';   % this is a vector of the variance of the errors
 
mx = inv(X'*X);
dmx = diag(mx); % we’re interested in standard errors,
 
% the diagonals of the covariance matrix of bs
siga = (s2*dmx(1)).^0.5;      % std err of alpha, beta
sigb = (s2*dmx(2:end)').^0.5; % s2 is a column vector of 25. dmx’ is a
                              % row vector corresponding to factors.
                              % this produces a matrix the same size as
                              % the b coefficients.
sig_beta = sigb(:,1);
sig_h = sigb(:,2);
sig_s = sigb(:,3);
 
R2 = 1-s2./(std(rx).^2)';
 
% Pull out the regression factors
ff_alpha = b(1,:);
ff_beta = b(2,:);
h = b(3,:);
s = b(4,:);
 
% Calculate Arithmetic Mean for each of 25 portfolios over range
arithmeans = mean(r);
 
% Calculate Geometric Mean for each of 25 portfolios over selected range
georeturns = r./100 + 1;
geomeans = 100*(exp(mean(log(georeturns)))-1);
 
% Select if Geometric or Arithmetic mean is used by adjusting comments
%meanreturns = arithmeans;  % uncomment to use arithmetic means
meanreturns = geomeans;   % uncomment to use geometric means
 
% Expand 5x5 data to 10x10 for use in surface plot function
returns = [meanreturns ; meanreturns];
returns = reshape(returns,10,5);
returns = [returns;returns]
returns = reshape(returns,10,10);
 
% beta can be used for surface plot of beta
beta_ff = [ff_beta;ff_beta]
beta_ff = reshape(beta_ff,10,5);
beta_ff = [beta_ff;beta_ff];
beta_ff = reshape(beta_ff,10,10);
 
% s; s_ff can be used for surface plot of size factor
s_ff = [s;s]
s_ff = reshape(s_ff,10,5);
s_ff = [s_ff;s_ff];
s_ff = reshape(s_ff,10,10);
 
% h; h_ff can be used for surface plot of value factor
h_ff = [h;h]
h_ff = reshape(h_ff,10,5);
h_ff = [h_ff;h_ff];
h_ff = reshape(h_ff,10,10);
 
% Define x and y values
x = [0 0.999 1 1.999 2 2.999 3 3.999 4 5];
y = [0 0.999 1 1.999 2 2.999 3 3.999 4 5];
 
% Create x-y mesh for surface plot
[xx,yy] = meshgrid(x,y);
 
% Generate Plot
surf(xx,yy,returns)
xlabel('Size','fontsize',20)
ylabel('Value','fontsize',20)
%zlabel('Arithmetic Average Monthly Return (%)','rotation',90,'fontsize',20)
%zlabel('Geometric Average Monthly Return (%)','rotation',90,'fontsize',20)
title(plottitle,'fontsize',36)
axis([0 5 0 5 min(0,min(meanreturns)-.1) max(2,max(meanreturns)+0.01)])
 
% Size Lables for corner portfolios
line([4.5 4.5],[0.5 0.5],[meanreturns(21) meanreturns(21)+0.1])
text(4.5,0.5,meanreturns(21)+0.15,'LG','horizontalalignment','center','fontsize',18)
line([4.5 4.5],[4.5 4.5],[meanreturns(25) meanreturns(25)+0.1])
text(4.5,4.5,meanreturns(25)+0.15,'LV','horizontalalignment','center','fontsize',18)
line([0.5 0.5],[4.5 4.5],[meanreturns(5) meanreturns(5)+0.1])
text(0.5,4.5,meanreturns(5)+0.15,'SV','horizontalalignment','center','fontsize',18)
line([0.5 0.5],[0.5 0.5],[meanreturns(1) meanreturns(1)+0.1])
text(0.5,0.5,meanreturns(1)+0.15,'SG','horizontalalignment','center','fontsize',18)
 
% ETFs
line([4.55 4.55],[1.1 1.1],[meanreturns(22) meanreturns(22)+0.15])
text(4.55,1.1,meanreturns(22)+0.18,'SPY','horizontalalignment','center','fontsize',18)
 
line([4.75 4.75],[1.25 1.25],[meanreturns(22) meanreturns(22)+0.1])
text(4.75,1.25,meanreturns(22)+0.15,'DIA','horizontalalignment','center','fontsize',18)
 
line([3.5 3.5],[0.1 0.1],[meanreturns(16) meanreturns(16)+0.1])
text(3.5,0.1,meanreturns(16)+0.15,'QQQQ','horizontalalignment','center','fontsize',18)
 
line([4.5 4.5],[1.5 1.5],[meanreturns(22) meanreturns(22)+0.18])
text(4.50,1.5,meanreturns(22)+0.2,'IVE*','horizontalalignment','center','fontsize',18)
 
line([1.6 1.6],[1.7 1.7],[meanreturns(7) meanreturns(7)+0.25])
text(1.6,1.7,meanreturns(7)+0.3,'IWM*','horizontalalignment','center','fontsize',18)
 
line([1.7 1.7],[3.55 3.55],[meanreturns(9) meanreturns(9)+0.1])
text(1.7,3.55,meanreturns(9)+0.15,'IWN','horizontalalignment','center','fontsize',18)
 
line([1.65 1.65],[1.85 1.85],[meanreturns(7) meanreturns(7)+0.1])
text(1.65,1.85,meanreturns(7)+0.15,'IJR','horizontalalignment','center','fontsize',18)
 
line([1.6 1.6],[2.85 2.85],[meanreturns(8) meanreturns(8)+0.1])
text(1.6,2.85,meanreturns(8)+0.15,'IJS','horizontalalignment','center','fontsize',18)
 
line([2.9 2.9],[4.6 4.6],[meanreturns(15) meanreturns(15)+0.1])
text(2.9,4.6,meanreturns(15)+0.15,'IYR','horizontalalignment','center','fontsize',18)
 
line([3.4 3.4],[1.25 1.25],[meanreturns(19) meanreturns(19)+0.1])
text(3.4,1.25,meanreturns(19)+0.15,'MDY','horizontalalignment','center','fontsize',18)
 
% Color range set from 0 to 1.6 rather than allowing autoscale.
% This is done for easier comparison between plots, but colors will
% max out for values above 1.6 or below 0.
% For arithmetic averages, I think a range of 0 to 2 works better
caxis([0 1.6]);
view(50, 25);
% top view
%view(270,90);
replot