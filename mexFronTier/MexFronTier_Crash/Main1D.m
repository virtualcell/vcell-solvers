VelBoundaryCond = {'RankineHugoniot';
                   'ZeroFlux';
                   'ZeroDerivative';
                   'ZeroDirichlet'};

config.NumNodesX = 1000;

M_tot = 1;
BoundaryLeft = -2.5;
BoundaryRight = -1;
DomainSize = 6;
MeshSize = DomainSize/config.NumNodesX;
InitCondition.M = @(x) 2*M_tot*(BoundaryRight-x)/(BoundaryRight - BoundaryLeft)^2;%exp(-x+BoundaryLeft)/4*exp(1); %exp(-8*x.^2)+0.01*rand(size(x)); %exp(-abs(x-0.01))/4*exp(1);%
InitCondition.U = @(x) 0;

config.VelBoundaryCond = VelBoundaryCond{1};
config.returnAllData   = false;
config.Visualization   = true;
config.Domain          = [-DomainSize/2 DomainSize/2];
config.Front           = [BoundaryLeft BoundaryRight];
config.InitCondition   = InitCondition;
config.T_max           = 9;
% config.V0              = @(t) 1.*(t<30)+0.6*(t>=30).*(t<40)+1.*(t>=40);
config.V0              = @(t) 1;
config.D               = 0.2;                        
config.Du              = 0.6;                        
config.ksi             = 0.5;                        
config.k               = 0;                        
config.M0              = 1;
config.alpha           = .3;
config.beta            = 1;

tic;
[U_All, M_All, Time_All, PosX, Front_All] = GetSolution1D(config);
toc

L = Front_All(2)-Front_All(1);
v = U_All(find(U_All>0,1,'first')) - config.V0(0)/L;

ksi_bar = config.ksi*L^2/config.Du;
v_bar   = v*L/config.Du;
d       = config.Du/config.D;

lambda1 = 1/2*(sqrt(v_bar^2 + 4*ksi_bar) + v_bar);
lambda2 = 1/2*(sqrt(v_bar^2 + 4*ksi_bar) - v_bar);
A       = 1/(v_bar^2*d*(d-1) - ksi_bar);
B       =  - ksi_bar;
C       = v_bar*d;

s2      = exp(lambda2); 
s1      = exp(-lambda1); 
s       = exp(-C);

b       = A*(B*((s2-s)/(s2-s1)/lambda2+(s1-s)/(s2-s1)/lambda1)+C);
a       = A*(B*(s1*(s2-s)/(s2-s1)/lambda2+s2*(s1-s)/(s2-s1)/lambda1)+C*s);

m0_bar  = 2*v_bar/(a+b);
v0_bar  = v_bar*(b-a)/(b+a);
M_tot_wave = config.Du^2/L*m0_bar*(1-exp(-v_bar*d))/v_bar/d;    

L_assimp = 2*config.V0(0)/M_tot;
v_assimp = config.V0(0)/L_assimp;