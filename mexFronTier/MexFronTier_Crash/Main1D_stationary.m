VelBoundaryCond = {'RankineHugoniot';
                   'ZeroFlux';
                   'ZeroDerivative';
                   'ZeroDirichlet'};

config.NumNodesX = 2000;

M_tot = 1;
DomainSize = 8;
BoundaryLeft = -DomainSize/2+1;
BoundaryRight = BoundaryLeft+1;
% DomainSize = 30;
MeshSize = DomainSize/config.NumNodesX;
InitCondition.M = @(x) M_tot/(BoundaryRight-BoundaryLeft);%2*M_tot*(BoundaryRight-x)/(BoundaryRight - BoundaryLeft)^2;%exp(-x+BoundaryLeft)/4*exp(1); %exp(-8*x.^2)+0.01*rand(size(x)); %exp(-abs(x-0.01))/4*exp(1);%
InitCondition.U = @(x) 0;

% config.VelBoundaryCond = VelBoundaryCond{3};
% config.returnAllData   = false;
% config.Visualization   = true;
% config.Domain          = [-DomainSize/2 DomainSize/2];
% config.Front           = [BoundaryLeft BoundaryRight];
% config.InitCondition   = InitCondition;
% config.T_max           = 100;
% % config.V0              = @(t) 1.*(t<30)+0.6*(t>=30).*(t<40)+1.*(t>=40);
% config.V0              = @(t) 1;
% config.D               = 0.2;                        
% config.Du              = 0.5;                        
% config.ksi             = 0.5;                        
% config.k               = 1;                        
% config.M0              = 1;
% config.alpha           = .3;
% config.beta            = 1;

config.VelBoundaryCond = VelBoundaryCond{1};
config.returnAllData   = false;
config.Visualization   = true;
config.Domain          = [-DomainSize/2 DomainSize/2];
config.Front           = [BoundaryLeft BoundaryRight];
config.InitCondition   = InitCondition;
config.T_max           = 10;
config.V0              = @(t) 1;
config.D               = 0.2;                        
config.Du              = 0.5;                        
config.ksi             = 1;                        
config.k               = 0;                        
config.M0              = 1;
config.alpha           = .3;
config.beta            = 1;

% m0 = InitCondition.M(0);
% L = BoundaryRight-BoundaryLeft;
V0 = config.V0(0);
a = V0*sqrt(config.Du*config.ksi)/M_tot;
L = sqrt(config.Du/config.ksi)*log((1+a)/(1-a));

tic;
[U_All, M_All, Time_All, PosX, Front_All] = GetSolution1D(config);
toc

[L Front_All(2)-Front_All(1)]


    
