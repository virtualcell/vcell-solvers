function [U_All, M_All, Time_All, PosX, Front_All] = GetSolution1D(config)

ticID = tic;


% Set up parameters

NumNodesX       = config.NumNodesX;
Domain          = config.Domain;
V0              = config.V0;
D               = config.D;                        
Du              = config.Du;                        
ksi             = config.ksi;                        
k               = config.k;     
M0              = config.M0;
alpha           = config.alpha;
beta            = config.beta;
InitCondition   = config.InitCondition;
T_max           = config.T_max; 
BoundaryLeft    = config.Front(1);
BoundaryRight   = config.Front(2);
VelBoundaryCond = config.VelBoundaryCond;

MeshSize = (Domain(2) - Domain(1))/NumNodesX;
PosX = Domain(1)+MeshSize/2:MeshSize:Domain(2)-MeshSize/2;
PosX = PosX(:);

dt = MeshSize^2/max(D, Du)/4;
NumTimeNodes = round(T_max/dt);
dt = T_max/NumTimeNodes;

if config.returnAllData
    U_All = zeros(NumNodes, NumTimeNodes);
    M_All = zeros(NumNodes, NumTimeNodes);
    Time_All = zeros(1, NumTimeNodes);
    Front_All = zeros(2, NumTimeNodes);
end

% lambda_M = D*dt/MeshSize^2;
% nu_M = D*dt/MeshSize;
% lambda_U = Du*dt/MeshSize^2;
% nu_U = Du*dt/MeshSize;

% Initialize front, mesh & solution

InsideMask = (PosX>BoundaryLeft).*(PosX<BoundaryRight);
M = InsideMask.*InitCondition.M(PosX);
U = InsideMask.*InitCondition.U(PosX);
InsideOldIdx = find(InsideMask);
vol_b1_Old = MeshSize/2 + PosX(InsideOldIdx(1)) - BoundaryLeft;
vol_b2_Old = MeshSize/2 + BoundaryRight - PosX(InsideOldIdx(end));
Vol_Old = [vol_b1_Old; MeshSize*ones(length(InsideOldIdx)-2,1); vol_b2_Old];

sum(M(InsideOldIdx).*Vol_Old)
%Mass = [];

fprintf('\n')
strlength = 0;
for iT = 1:NumTimeNodes
    if ~mod(iT,round(NumTimeNodes/50))
        strlength = DisplayProgress(ticID, iT, NumTimeNodes, strlength);
    end
    %Mass = [Mass, sum(M(InsideOldIdx).*Vol_Old)];
% Propogate front    
    
    time = dt*iT;
    TotalLength = BoundaryRight - BoundaryLeft;
%     BoundaryLeft = BoundaryLeft + dt;
%     BoundaryRight = BoundaryRight + dt;
    if strcmp(VelBoundaryCond, 'ZeroDirichlet')
        VelBoundaryLeft = -(beta/(M(InsideOldIdx(1))+M0) - alpha*TotalLength);
        VelBoundaryRight = beta/(M(InsideOldIdx(end))+M0) - alpha*TotalLength;
    else
        VelBoundaryLeft = U(InsideOldIdx(1)) - V0/TotalLength;
        VelBoundaryRight = U(InsideOldIdx(end)) + V0/TotalLength;
    end
    BoundaryLeft = BoundaryLeft + VelBoundaryLeft*dt;
    BoundaryRight = BoundaryRight + VelBoundaryRight*dt;
    if BoundaryRight>Domain(2) || BoundaryLeft<Domain(1)
        break;
    end
    
% Identify inside nodes
    
   InsideMask = (PosX>BoundaryLeft).*(PosX<BoundaryRight);
   InsideIdx = find(InsideMask);

% Update volumes near the boundary

    vol_b1 = MeshSize/2 + PosX(InsideOldIdx(1)) - BoundaryLeft;
    vol_b2 = MeshSize/2 + BoundaryRight - PosX(InsideOldIdx(end));
    NumInsideNodes = length(InsideOldIdx);
    Vol = [vol_b1; MeshSize*ones(NumInsideNodes-2,1); vol_b2];
    
% Update concentrations at the old nodes due to diffusion 
    
    M_local = M(InsideOldIdx);
    U_local = U(InsideOldIdx);
        
    M_flux = -k*[0; (U_local(1:end-1) + U_local(2:end)).*(M_local(1:end-1) + M_local(2:end))/4; 0] +... 
             -D*[0; (M_local(1:end-1) - M_local(2:end))/MeshSize; 0];
    U_flux =  [0; (M_local(1:end-1) + M_local(2:end))/2; 0] +... 
             -Du*[0; (U_local(1:end-1) - U_local(2:end))/MeshSize; 0];
    
    M_local = (M_local.*Vol_Old + dt* (M_flux(2:end)-M_flux(1:end-1)))./Vol;
    switch VelBoundaryCond
        case 'RankineHugoniot'
            U_local = (U_local.*Vol_Old + dt* (U_flux(2:end)-U_flux(1:end-1)))./Vol - dt*ksi*U_local;
        case 'ZeroFlux'
            U_local = (U_local.*Vol + dt* (U_flux(2:end)-U_flux(1:end-1)))./Vol - dt*ksi*U_local;
        case 'ZeroDerivative'
            U_local(2:end-1) = U_local(2:end-1)*(1-dt*ksi) + dt* (U_flux(3:end-1)-U_flux(2:end-2))/MeshSize ;
            U_local(1) = U_local(2);
            U_local(end) = U_local(end-1);
        case 'ZeroDirichlet'
            U_local(2:end-1) = U_local(2:end-1)*(1-dt*ksi) + dt* (U_flux(3:end-1)-U_flux(2:end-2))/MeshSize ;
            U_local(1) = 0;
            U_local(end) = 0;
    end
    
%     U_local_tmp(1) = U_local(1)*vol_b1_Old/vol_b1 + nu/vol_b1*(U_local(2)-U_local(1));
%     U_local_tmp(NumInsideNodes) = U_local(NumInsideNodes)*vol_b2_Old/vol_b2 + nu/vol_b2*(U_local(NumInsideNodes-1) - U_local(NumInsideNodes));
%     U_local = U_local_tmp;
    
    M = zeros(NumNodesX,1);
    M(InsideOldIdx) = M_local;
    U = zeros(NumNodesX,1);
    U(InsideOldIdx) = U_local;
    
% Adjust concentaration at the nodes neighboring those that disappear and new nodes     
    
    if InsideIdx(1) > InsideOldIdx(1)
        vol_b1_Old = vol_b1;
        vol_b1 = MeshSize/2 + PosX(InsideIdx(1)) - BoundaryLeft;
        gone_node = InsideOldIdx(1);
        M(gone_node+1) = (M(gone_node+1)*MeshSize + M(gone_node)*vol_b1_Old)/vol_b1;
        M(gone_node) = 0;
        U(gone_node+1) = (U(gone_node+1)*MeshSize + U(gone_node)*vol_b1_Old)/vol_b1;
        U(gone_node) = 0;
    elseif InsideIdx(1) < InsideOldIdx(1)
        new_node = InsideIdx(1);
        vol_b1 = MeshSize/2 + PosX(InsideIdx(1)) - BoundaryLeft;
        M(new_node)=M(new_node+1);
        U(new_node)=U(new_node+1);
    end;
    if InsideIdx(end) < InsideOldIdx(end)
        vol_b2_Old = vol_b2;
        vol_b2 = MeshSize/2 + BoundaryRight - PosX(InsideIdx(end));
        gone_node = InsideOldIdx(end);
        M(gone_node-1) = (M(gone_node-1)*MeshSize + M(gone_node)*vol_b2_Old)/vol_b2;
        M(gone_node) = 0;
        U(gone_node-1) = (U(gone_node-1)*MeshSize + U(gone_node)*vol_b2_Old)/vol_b2;
        U(gone_node) = 0;
    elseif InsideIdx(end) > InsideOldIdx(end)
        new_node = InsideIdx(end);
        vol_b2 = MeshSize/2 + BoundaryRight - PosX(InsideIdx(end));
        M(new_node)=M(new_node-1);
        U(new_node)=U(new_node-1);
    end;
    Vol = [vol_b1; MeshSize*ones(length(InsideIdx)-2,1); vol_b2];
    
% Collect the results    

    if config.returnAllData
        U_All(:,iT) = U;
        M_All(:,iT) = M;
        Time_All(iT) = time;
        Front_All(:,iT) = [BoundaryLeft BoundaryRight];
    end
    
% Visualize solution    

    if config.Visualization && ~mod(iT,round(NumTimeNodes/100))
        subplot(3,1,1);
        plot(PosX(InsideIdx),M(InsideIdx),'k.-');
%         ylim([0 8])
        title('Myosin concentration')
        xlim(Domain)
        
        subplot(3,1,2);
        plot(PosX(InsideIdx),U(InsideIdx),'k.-');
        title('Velocity of actin flow')
        xlim(Domain)
        
        subplot(3,1,3);
        plot([BoundaryLeft BoundaryRight], [0 0],'r', 'LineWidth', 3);
        hold on
        quiver([BoundaryLeft BoundaryRight], [0 0], [VelBoundaryLeft VelBoundaryRight],  [0 0])
        text(BoundaryLeft-(Domain(2) - Domain(1))/10, 0.1, num2str(abs(VelBoundaryLeft),3));
        text(BoundaryRight+(Domain(2) - Domain(1))/10, 0.1, num2str(abs(VelBoundaryRight),3));
        hold off
        title('Shape')
        xlim(Domain)
        ylim([-0.5 0.5])

        pause(0.01);        
    end;

    InsideOldIdx = InsideIdx;
    Vol_Old = Vol;
end;

if ~config.returnAllData
    U_All = U;
    M_All = M;
    Time_All = time;
    Front_All = [BoundaryLeft BoundaryRight];
end
