clear; clc;

Lakers = main('lakersoffense.csv','heatdefense.csv')
Heat = main('heatoffense.csv','lakersdefense.csv')

%This function reads/converts csv files into vectors containing the 
%statistics of the teams. 

function [fgm,fga,tpm,tpa,ftm,fta] = vect(x)
y = table2array(readtable(x));
fgm = sort(y(:,1))';
fga = sort(y(:,2))';
tpm = sort(y(:,3))';
tpa = sort(y(:,4))';
ftm = sort(y(:,5))';
fta = sort(y(:,6))';
end

%This function takes in the attempted shots from team 1's offensive data +
%team 2's defensive data & calculate the expected field goal attempts 
%through that given game. 
%efga = expected field goal attempts.

function efga = ava(m,n)
m1 = mean(m,'all');
m2 = mean(n,'all');
efga = (m1 + m2)/2;
end

%This function takes the input of 2 variables & perform linear regression
%to see the line of best fit through the data. 
%Then it will predict the field goals made given the expected field goal
%attempts. 

function f = linreg(t,y,x)
g = linspace(10,100,100);
[r,m,b] = regression(t,y);
f = x*m + b + r; %Expected FG's made
c = g.*m + b +r; %Trend line
plot(g,c); hold on; plot(x,f,'o');
end

%This function calculates the expected final score, given the amount of
%expected field goals, 3 pointers & free throws. Obviously, the team with
%the higher score wins. 
%Each FG is 2 points. Each team will get an additional point for every 3-pt
%shot and free throw made. 

function s = score(fg,three,ft)
s = ceil(fg*2 + three + ft);
end 

%Main function that bridges all the other functions to produce
%the final expected score.

function final_score = main(t1,t2)
[fgm1,fga1,tpm1,tpa1,ftm1,fta1] = vect(t1);
[fgm2,fga2,tpm2,tpa2,ftm2,fta2] = vect(t2);

%Calculating the expected attempts for the categories of FG's, 3pt's + FT's
%Also creating 3 subplots of the offensive data for both teams.

subplot(3,1,1)
title('Expected FG Attempts vs. Makes');
xlabel('FG Attempts'), ylabel('FG Makes');

efga = ava(fga1,fga2);
efgm = linreg(fga1,fgm1,efga);

subplot(3,1,2)
title('Expected 3PT Attempts vs. Makes');
xlabel('3PT Attempts'), ylabel('3PT Makes');

etpa = ava(tpa1,tpa2);
etpm = linreg(tpa1,tpm1,etpa);

subplot(3,1,3)
title('Expected FT Attempts vs. Makes');
xlabel('FT Attempts'), ylabel('FT Makes');

efta = ava(fta1,fta2);
eftm = linreg(fta1,ftm1,efta);

legend("Lakers' Line", "Lakers' EFG", "Heat's Line","Heat's EFG");

% Estimate the final score for each team given the expected attempts + makes
% for FG's, 3-pt's & FT's

final_score = score(efgm,etpm,eftm);
end







