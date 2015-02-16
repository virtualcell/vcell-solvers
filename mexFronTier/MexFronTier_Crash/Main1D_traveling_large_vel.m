VelBoundaryCond = {'RankineHugoniot';
                   'ZeroFlux';
                   'ZeroDerivative';
                   'ZeroDirichlet'};

config.NumNodesX = 1000;

M_tot = .1;

config.VelBoundaryCond = VelBoundaryCond{1};
config.returnAllData   = false;
config.Visualization   = true;
config.V0              = 0.125;
config.D               = 0.005;                        
config.Du              = 0.5;                        
config.ksi             = 0.5;                        
config.k               = 0;                        
config.M0              = 1;
config.alpha           = .3;
config.beta            = 1;

L = 2*config.Du*config.V0/M_tot;
v = M_tot/2/config.Du;

k = v/config.D;
m0 = M_tot*k/(1-exp(-k*L));

ksi_bar = config.ksi*L^2/config.Du;
v_bar   = v*L/config.Du;
v0_bar   = config.V0/config.Du;
d       = config.Du/config.D;


DomainSize = 10*L;
MeshSize = DomainSize/config.NumNodesX;

BoundaryLeft = -DomainSize/2.2;
BoundaryRight = BoundaryLeft+L;

InitCondition.M = @(x) m0*exp(-k*(x-BoundaryLeft));%exp(-x+BoundaryLeft)/4*exp(1); %exp(-8*x.^2)+0.01*rand(size(x)); %exp(-abs(x-0.01))/4*exp(1);%
InitCondition.U = @(x) 0;
config.InitCondition   = InitCondition;
config.Front           = [BoundaryLeft BoundaryRight];
config.T_max           = DomainSize/v;
config.Domain          = [-DomainSize/2 DomainSize/2];


tic;
[U_All, M_All, Time_All, PosX, Front_All] = GetSolution1D(config);
toc

L_num = Front_All(2)-Front_All(1);
v_num = U_All(find(abs(U_All)>0,1,'last')) + config.V0/L_num;

[L_num L; v_num v]

