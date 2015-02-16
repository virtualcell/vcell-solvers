VelBoundaryCond = {'RankineHugoniot';
                   'ZeroFlux';
                   'ZeroDerivative';
                   'ZeroDirichlet'};

config.NumNodesX = 30;

M_tot = 100;

config.VelBoundaryCond = VelBoundaryCond{1};
config.returnAllData   = false;
config.Visualization   = true;
config.V0              = @(t) .2;
config.D               = 1;                        
config.Du              = 0.5;                        
config.ksi             = 0.5;                        
config.k               = 0;                        
config.M0              = 1;
config.alpha           = .3;
config.beta            = 1;

% L = 12*config.Du^2/M_tot/(config.Du/config.D+1);
% ksi_bar = config.ksi*L^2/config.Du;
% 
% v = 2*config.D^2/config.Du/L*(1-config.V0(0)/M_tot*sqrt(config.Du*config.ksi)/tanh(1/2*sqrt(ksi_bar)));


v_bar   = v*L/config.Du;
d       = config.Du/config.D;

lambda1 = 1/2*(sqrt(v_bar^2 + 4*ksi_bar) + v_bar);
lambda2 = 1/2*(sqrt(v_bar^2 + 4*ksi_bar) - v_bar);
A       = 1/(v_bar^2*d*(d-1) - ksi_bar);
B       = - ksi_bar;
C       = v_bar*d;

s2      = exp(lambda2); 
s1      = exp(-lambda1); 
s       = exp(-C);

b       = A*(B*((s2-s)/(s2-s1)/lambda2+(s1-s)/(s2-s1)/lambda1)+C);
a       = A*(B*(s1*(s2-s)/(s2-s1)/lambda2+s2*(s1-s)/(s2-s1)/lambda1)+C*s);

m0_bar = 2*v_bar/(a+b);
v0_bar = v_bar*(b-a)/(b+a);

% config.V0 = @(t) v0_bar*config.Du;
% M_tot = config.Du^2/L*m0_bar*(1-exp(-v_bar*d))/v_bar/d;

k = v/config.D;
m0 = M_tot*k/(1-exp(-k*L));

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

L = Front_All(2)-Front_All(1);
v = U_All(find(abs(U_All)>0,1,'last')) + config.V0(0)/L;

L_assimp = 2*config.Du*config.V0(0)/M_tot;
v_assimp = M_tot/2/config.Du;
[L L_assimp; v v_assimp]

