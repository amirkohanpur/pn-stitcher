% 
% This Matlab script produces Kr-Sw and Pc-Sw curves
% You need to specify prefix, label, and address of the PN data
% The input is the CSV file comes from running PoreFlow on a PN
% Uncomment lines to produce comparative plots of two PNs
% Code by Amir Kohanpur (kohanpu2@illinois.edu)
% 

clear;
close all;

rockname1 = 'mtsimon-s';
%rockname2 = 'mtsimon-lr';
rockaddress1 = '../simulation/';
%rockaddress2 = '../reference/';
label1 = 'Stitched';
%label2 = 'Reference';

label_w = ', w';
label_n = ', nw';
label1_w = strcat(label1, label_w);
label1_n = strcat(label1, label_n);
%label2_w = strcat(label2, label_w);
%label2_n = strcat(label2, label_n);

drainage1 = importdata(strcat(rockaddress1, rockname1, '_draincycle_1.csv'));
%drainage2 = importdata(strcat(rockaddress2, rockname2, '_draincycle_1.csv'));

Sw_drainage1 = drainage1.data(:,1);
Pc_drainage1 = drainage1.data(:,2);
Krw_drainage1 = drainage1.data(:,3);
Krnw_drainage1 = drainage1.data(:,4);

%Sw_drainage2 = drainage2.data(:,1);
%Pc_drainage2 = drainage2.data(:,2);
%Krw_drainage2 = drainage2.data(:,3);
%Krnw_drainage2 = drainage2.data(:,4);

figure(1);
hold on;
pkdr11 = plot(Sw_drainage1, Krw_drainage1, '-b', 'LineWidth',2);
pkdr12 = plot(Sw_drainage1, Krnw_drainage1, '-r', 'LineWidth',2);
%pkdr21 = plot(Sw_drainage2, Krw_drainage2, '--b', 'LineWidth',2);
%pkdr22 = plot(Sw_drainage2, Krnw_drainage2, '--r', 'LineWidth',2);
%title('Relative Permeability Curve in Drainage');
xlabel('Wetting phase saturation');
ylabel('Relative permeability');
legend([pkdr11 pkdr12],{label1_w,label1_n},'Location','north');
%legend([pkdr11 pkdr12 pkdr21 pkdr22],{label1_w,label1_n,label2_w,label2_n},'Location','north');
axis([0 1 0 1]);
box on; legend boxoff;
set(gca,'xcolor','k'); 
set(gca,'ycolor','k');
set(gca,'FontName','Times New Roman');
set(gca,'FontSize',18);
print('curve_Kr.png','-dpng');

figure(3);
hold on;
ppdr1 = plot(Sw_drainage1, Pc_drainage1, '-k', 'LineWidth',2);
%ppdr2 = plot(Sw_drainage2, Pc_drainage2,'--k', 'LineWidth',2);
%title('Capillary Pressure Curve in Drainage');
xlabel('Wetting phase saturation');
ylabel('Capillary pressure (Pa)');
legend(ppdr1,label1,'Location','northeast');
%legend([ppdr1 ppdr2],{label1,label2},'Location','northeast');
box on; legend boxoff;
axis([0 1 0 inf]);
set(gca,'yscale','log');
set(gca,'xcolor','k'); 
set(gca,'ycolor','k');
set(gca,'FontName','Times New Roman');
set(gca,'FontSize',18);
print('curve_Pc.png','-dpng');

close all;
