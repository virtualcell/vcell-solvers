VelBoundaryCond = {'RankineHugoniot';
                   'ZeroFlux';
                   'ZeroDerivative';
                   'ZeroDirichlet'};

config.NumNodesX = 1600;

config.VelBoundaryCond = VelBoundaryCond{1};
config.returnAllData   = false;
config.Visualization   = true;
config.D               = 0.1;                        
config.Du              = 0.5;                        
config.ksi             = 1;                        
config.k               = 0;                        
config.M0              = 1;
config.alpha           = .3;
config.beta            = 1;

L = 1;
v = .1;

ksi_bar = config.ksi*L^2/config.Du;
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

config.V0 = v0_bar*config.Du;
M_tot = config.Du^2/L*m0_bar*(1-exp(-v_bar*d))/v_bar/d;

k = v/config.D;
m0 = M_tot*k/(1-exp(-k*L));

lam1 = 1/2/config.Du*(sqrt(v^2 + 4*config.ksi*config.Du) + v);
lam2 = 1/2/config.Du*(sqrt(v^2 + 4*config.ksi*config.Du) - v);
U1 = -m0*config.ksi/(config.Du*k^2-v*k-config.ksi)/lam2/config.Du*(exp(lam2*L)-exp(-k*L))/(exp(lam2*L)-exp(-lam1*L));
U2 = -m0*config.ksi/(config.Du*k^2-v*k-config.ksi)/lam1/config.Du*(exp(-lam1*L)-exp(-k*L))/(exp(lam2*L)-exp(-lam1*L));
K = m0*k/(config.Du*k^2-v*k-config.ksi);

DomainSize = 20*L;
MeshSize = DomainSize/config.NumNodesX;

BoundaryLeft = -DomainSize/2.4;
BoundaryRight = BoundaryLeft+L;

InitCondition.M = @(x) m0*exp(-k*(x-BoundaryLeft));
InitCondition.U = @(x) U1*exp(-lam1*(x-BoundaryLeft))+U2*exp(lam2*(x-BoundaryLeft))+K*exp(-k*(x-BoundaryLeft));%0;
config.InitCondition   = InitCondition;
config.Front           = [BoundaryLeft BoundaryRight];
config.T_max           = DomainSize/v;
config.Domain          = [-DomainSize/2 DomainSize/2];


tic;
[U_All, M_All, Time_All, PosX, Front_All] = GetSolution1D(config);
toc

L_res = Front_All(2)-Front_All(1);
v_res = U_All(find(abs(U_All)>0,1,'last')) + config.V0/L_res;

[L_res L; v_res v]

% figure
% plot(PosX, InitCondition.U(PosX-Front_All(1)+BoundaryLeft).*(PosX<=Front_All(2)).*(PosX>=Front_All(1)))
% hold on
% plot(PosX,U_All,'k.-');
% hold off

