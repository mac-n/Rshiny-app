x=1:15;
data=[19 20 24 16 18 16 16 17 19 13 20 19 23 20 20];
%this contains some massive kludges and will be confusing af to read if
%you're not me and possibly even if you are
%in future use a different h handle for every set of bars you want
%appearing in the legend. 
data1=[ 0.8903004 0.8871550 0.8811496 0.8695438 0.8855256 0.8719803 0.8639090 0.8708713 0.8620105 0.8694474 0.8880721 0.8860159 0.8847440 0.8762717 0.8848090];
yyaxis left;
fig = figure;
left_color = [0 0 0];
right_color = [0 0 0];
set(fig,'defaultAxesColorOrder',[left_color; right_color;left_color]);
dummydata=[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0];
totaldata=[data1(:), dummydata(:)];
timedata=[dummydata(:),data(:)];
alldata=[data1;dummydata];

%bar(data1,'FaceColor',[0.6,0.6,0.6],'LineWidth',2);
h1=bar(x,totaldata,'FaceColor','flat','LineWidth',2);
%h1(2).FaceColor= [0 0.2 0.9]; % group 1 1st bar
% for i = 1:5
%     h1(1).CData((i),:) = [0.5882    0.4392    0.6235]; % group 1 1st bar
%     h1(1).CData((i+5),:) = [0.1294    0.5647    0.5490]; % group 1 1st bar
%     h1(1).CData(i+10,:) = [0.9922    0.9059    0.1451];
% end
h1(2).CData(1,:)=[0.9922    0.9059    0.1451];
for i = 1:5
    h1(1).CData((i+10),:) = [0.5882    0.4392    0.6235]; % group 1 1st bar
    h1(1).CData((i),:) = [0.1294    0.5647    0.5490]; % group 1 1st bar
    h1(1).CData(i+5,:) = [0.9922    0.9059    0.1451];
end
%1h1(1).CData(6,:) = [0.1294    0.5647    0.5490]; % group 1 1st bar
%(1).CData(1,:) = [0.9922    0.9059    0.1451]; % group 1 1st bar
%h1.CData(3,:) = [0 0 1]; 
%first five bars purple
%second five bars yellow 
%third five bars green
ylabel('Multiclass AUC');
set(gca,"YLim",[0.7 0.95]);
%set(gca,"Ytick,ticks",[0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9]);
%xtickangle(0);
set(gca,'LineWidth',3,'TickLength',[0.02,0.01],'TickDir','out');
set(gca,'FontSize',16);
box off;
%set(gca,"ylabel","Accuracy R^{2}");
%colororder({'k','k','k'});
%set(fig,'defaultAxesColorOrder',[0 0 0; 0 0 0]);

xticklabels({"CFS1" "CFS 2" "CFS 3" "CFS 4" "CFS 5" "CNS 1"  "CNS 2"  "CNS 3" "CNS 4" "CNS 5" "BR 1" "BR 2" "BR 3" "BR 4" "BR 5"});
xtickangle(45);
ycolor="black";
yl=yline(0.862,'--');
yl.LineWidth=2.5
%bar(totaldata);
%bar(alldata);
yyaxis right;
yticks('auto');
h2=bar(x,timedata,'FaceColor','flat','LineWidth',2);
h2(1).CData((1),:)=[0.5882    0.4392    0.6235];
for i =1:15
    h2(2).CData(i,:)=[0.6 0.6 0.6];
end
yl.LabelHorizontalAlignment='center';
 yl.FontSize=14
%set(gca, 'YScale', 'log');
set(gca,'LineWidth',3,'TickLength',[0.00,0.00],'TickDir','out');
ycolor="black";
t=ylabel("#features selected");
t.Color=[0.6 0.6 0.6];
h=legend([yl h1(1) h1(2) h2(1) h2(2) ],{'AUC - all features','AUC - CFS','AUC - Consistency (CNS)','AUC - Boruta (BR)', '#features selected' },'NumColumns',2,'FontSize',14,'Location','SouthOutside','LineWidth',1);
%egend(h, {'Dataset 1' 'Dataset 2' 'Dataset 3' 'Dataset 4'})