VelBoundaryCond = {'RankineHugoniot';
                   'ZeroFlux';
                   'ZeroDerivative';
                   'ZeroDirichlet'};

config.NumNodesX = 151;

DomainSizeX = 8;
MeshSize = DomainSizeX/config.NumNodesX;
Domain = {[-1.5 DomainSizeX-1.5],[-DomainSizeX/2 DomainSizeX/2]};
InitCondition.M = @(x,y) 2*exp(-x);%400*exp(-(x+1)*2);%20*(1-x);%exp(-(x.^2+y.^2))/2.5;%
InitCondition.Ux = @(x,y) 0;
InitCondition.Uy = @(x,y) 0;

config.VelBoundaryCond = VelBoundaryCond{1};
config.T_max           = 5;
config.TimeDiscrScheme = 'explicit';
config.returnAllData   = true;
config.NumFrames       = 8;
config.Visualization   = false;
config.V0              = 10;
config.D               = 0.1;                        
config.Du              = 0.5;                        
config.ksi             = 0.5;                        
config.k               = 1;                        
config.Domain          = Domain;
config.InitCondition   = InitCondition;
config.MeshSize        = MeshSize;
config.FrontFormula    = [];

tic;
[Ux_All, Uy_All, M_All, Time_All, PosX, PosY, Front_All, FrontVelocity_All, Global_Area_All] = GetSolution(config);
toc

dataMask = Time_All>0;
M_All = M_All(:,dataMask);
Ux_All = Ux_All(:,dataMask);
Uy_All = Uy_All(:,dataMask);
Time_All = Time_All(dataMask);
Front_All = Front_All(dataMask);
Global_Area_All = Global_Area_All(dataMask);

% saveas(gcf, ['Result_N=' num2str(config.NumNodesX) '_Tmax=' num2str(config.T_max) '.jpg'], 'jpg');
save([config.VelBoundaryCond '_N=' num2str(config.NumNodesX) '_T=' num2str(config.T_max) '.mat'])
CreateMovie(M_All, Ux_All, Uy_All, PosX, PosY, Time_All, Front_All, [config.VelBoundaryCond '_N=' num2str(config.NumNodesX) '_T=' num2str(config.T_max)], {[-1.5 6.5], [-4 4]}, MeshSize)
