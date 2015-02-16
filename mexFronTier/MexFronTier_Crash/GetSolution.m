function [Ux_All, Uy_All, M_All, Time_All, PosX, PosY, Front_All, FrontVelocity_All, Global_Area_All] = GetSolution(config)

global V0 FX FY NormalX NormalY TotalArea  DomainX DomainY;

ticID = tic;

V0              = config.V0;
Domain          = config.Domain;
D               = config.D;                        
Du              = config.Du;                        
ksi             = config.ksi;                        
k               = config.k;                        
InitCondition   = config.InitCondition;
T_max           = config.T_max; 
MeshSize        = config.MeshSize;
Front.Formula   = config.FrontFormula;
VelBoundaryCond = config.VelBoundaryCond;
NumFrames       = config.NumFrames;
ExactPolygon    = [];

DomainX = Domain{1};
DomainY = Domain{2};
NumNodesX = round((DomainX(2)-DomainX(1))/MeshSize);
NumNodesY = round((DomainY(2)-DomainY(1))/MeshSize);
NumNodes  = NumNodesX*NumNodesY;
PosX = repmat((1:NumNodesX)', 1, NumNodesY);
PosX = DomainX(1)+(PosX(:) - 1/2)*MeshSize;
PosY = repmat(1:NumNodesY, NumNodesX,1);
PosY = DomainY(1)+(PosY(:) - 1/2)*MeshSize;

if strcmp(config.TimeDiscrScheme, 'explicit')
    dt = MeshSize^2/max(D, Du)/4;
    NumTimeNodes = max(round(T_max/dt),1);
    dt = T_max/NumTimeNodes;
else
    MaxVelocity = 1;
    dt = MeshSize/MaxVelocity/10;
    NumTimeNodes = round(T_max/dt);
    dt = T_max/NumTimeNodes; 
end;
NumTimeNodes=round(T_max/dt);

if config.returnAllData
    Ux_All = sparse(NumNodes, NumFrames);
    Uy_All = sparse(NumNodes, NumFrames);
    M_All = sparse(NumNodes, NumFrames);
    Global_Area_All = sparse(NumNodes, NumFrames);
    Time_All = zeros(1, NumFrames);
    Front_All = cell(1, NumFrames);
    FrontVelocity_All = cell(1, NumFrames);
end

% Initialize front, mesh & solution     

[f, Front] = InitializeFront(Front, ExactPolygon, Domain, round(NumNodesX/2));
[Ux, Uy, M, InsideOldMask, InteriorOldMask, BoundaryOldMask] =...
    InitializeSolution(PosX, PosY, Front, InitCondition, NumNodesX);
[NatNeighborPairs, NatNeighborBoundaryPairs, Vertices, VoronoiCells, Global_Area, D_Inv, Directions, MidPoints] =...
    InitializeControlVolumes(PosX, PosY, InsideOldMask, InteriorOldMask, BoundaryOldMask, Front, MeshSize, NumNodesX, NumNodesY);

A_old = Global_Area(InsideOldMask);
NumInsideNodes = nnz(InsideOldMask);

fprintf('\n')
strlength = 0;
doVoronoi = false;
for iT = 1:NumTimeNodes
    time = dt*iT;
    if ~mod(iT,round(NumTimeNodes/100))
        strlength = DisplayProgress(ticID, iT, NumTimeNodes, strlength);
    end
    
    TotalArea = sum(A_old);
    Tangents = [Front.Polygon(end,:); Front.Polygon(1:end-1,:)] - [Front.Polygon(2:end,:); Front.Polygon(1,:)];
    Normals = [-Tangents(:,2), Tangents(:,1)]./repmat(sqrt(Tangents(:,1).^2+Tangents(:,2).^2),1,2);
    NormalX = TriScatteredInterp(Front.Polygon(:,1), Front.Polygon(:,2), Normals(:,1),'nearest');
    NormalY = TriScatteredInterp(Front.Polygon(:,1), Front.Polygon(:,2), Normals(:,2),'nearest');
    FX = TriScatteredInterp(PosX(InsideOldMask), PosY(InsideOldMask), Ux(InsideOldMask),'nearest');
    FY = TriScatteredInterp(PosX(InsideOldMask), PosY(InsideOldMask), Uy(InsideOldMask),'nearest');  
    
% Propogate front
    
    Front = PropogateFront(f, time, Front, ExactPolygon, Domain);

% Identify inside nodes    
    
    [InsideMask, InteriorMask, BoundaryMask] = IdentifyNodeTypes(PosX, PosY, Front, NumNodesX);
    InteriorCandidatesMask = InteriorMask & InteriorOldMask;
    InteriorCandidatesIdx = find(InteriorCandidatesMask);
    BoundaryCandidatesIdx = find(InsideOldMask & ~InteriorCandidatesMask);
    new_nodes = find(BoundaryMask & ~InsideOldMask);
    gone_nodes = find(BoundaryOldMask & ~InsideMask);
    
% Voronoi cell decomposition (Old nodes, new front)
% Natural neighbors array: [i j], voronoi control volumes: A_i, edges: S_ij, inverse distances between nat.neighbors: D_Inv_ij ] 
    
    if doVoronoi || ~isempty(gone_nodes) || ~isempty(new_nodes)
        NumInsideNodes = nnz(InsideOldMask);
        Global_Area = zeros(NumNodes,1);
        
% Calculate metrics for irregular Voronoi cells near the boundary
    
        [NatNeighborBoundaryPairs, Vertices, VoronoiCells] =...
            GetVoronoiDecomposition(BoundaryCandidatesIdx, PosX, PosY, InsideOldMask, [], NumNodesX, NumNodesY);
        [D_Inv_Boundary, Directions, MidPoints] = GetBoundaryIndependentMetrics(NatNeighborBoundaryPairs, PosX, PosY);
        
% Fill in numbers for "interior" regular Voronoi cells (squares)   

        NatNeighborPairs = [InteriorCandidatesIdx-1 InteriorCandidatesIdx
                                    InteriorCandidatesIdx+1 InteriorCandidatesIdx
                                    InteriorCandidatesIdx-NumNodesX InteriorCandidatesIdx
                                    InteriorCandidatesIdx+NumNodesX InteriorCandidatesIdx
                                    NatNeighborBoundaryPairs];
                                
        Global_Area(InteriorCandidatesMask) = ones(length(InteriorCandidatesIdx),1)*MeshSize*MeshSize;
        D_Inv = [1/MeshSize*ones(4*length(InteriorCandidatesIdx),1); D_Inv_Boundary];
    end;
    [S_Boundary, Area] = GetBoundaryVolumes(BoundaryCandidatesIdx, MeshSize, VoronoiCells, Vertices, NatNeighborBoundaryPairs, Directions, MidPoints, Front);
    S = [MeshSize*ones(4*length(InteriorCandidatesIdx),1); S_Boundary];
    Global_Area(BoundaryCandidatesIdx) = Area;
    
% "Local" enumeration of inside nodes 
    
    tmp = zeros(1, NumNodes);
    tmp(InsideOldMask) = 1:NumInsideNodes;
    I = tmp(NatNeighborPairs(:,1));
    J = tmp(NatNeighborPairs(:,2));
    
% Generate matrices for discretized system of equations

    VelocityField.M = [Ux(InsideOldMask) Uy(InsideOldMask)];
    VelocityField.Ux = [M(InsideOldMask) zeros(NumInsideNodes,1)];
    VelocityField.Uy = [zeros(NumInsideNodes,1) M(InsideOldMask)];

    II = NatNeighborPairs(:,1);
    JJ = NatNeighborPairs(:,2);
    NormalsX = PosX(JJ)-PosX(II);
    NormalsY = PosY(JJ)-PosY(II);
    Lengths = sqrt(NormalsX.*NormalsX + NormalsY.*NormalsY);
    Normals = [NormalsX./Lengths NormalsY./Lengths];
    NormalVelocities.M = sum((VelocityField.M(I,:)+VelocityField.M(J,:))/2.*Normals,2);
    NormalVelocities.Ux = sum((VelocityField.Ux(I,:)+VelocityField.Ux(J,:))/2.*Normals,2);
    NormalVelocities.Uy = sum((VelocityField.Uy(I,:)+VelocityField.Uy(J,:))/2.*Normals,2);
        
    M_B.M = 1/2*sparse(I, J, NormalVelocities.M.*S, NumInsideNodes,NumInsideNodes);
    M_B.M = 1/2*(M_B.M-M_B.M');
    FluxUx = sparse(I, J, NormalVelocities.Ux.*S, NumInsideNodes,NumInsideNodes);
    FluxUx = 1/2*(FluxUx-FluxUx');
    FluxUy = sparse(I, J, NormalVelocities.Uy.*S, NumInsideNodes,NumInsideNodes);
    FluxUy = 1/2*(FluxUy-FluxUy');
    
    M_A = sparse(I, J, S.*D_Inv, NumInsideNodes, NumInsideNodes);
    M_A = 1/2*(M_A+M_A');        
    
% Update concentrations and velocities at the old nodes due to diffusion & advection    

    A = Global_Area(InsideOldMask);
    Ux_local = Ux(InsideOldMask);
    Uy_local = Uy(InsideOldMask);
    M_local = M(InsideOldMask);
    switch VelBoundaryCond
        case 'RankineHugoniot'
            Ux_local = ((sparse(1:NumInsideNodes, 1:NumInsideNodes, A_old*(1 - dt*ksi) - dt*sum(Du*M_A,2), NumInsideNodes, NumInsideNodes) + dt*Du*M_A)*Ux_local + dt*sum(FluxUx,2))./A;
            Uy_local = ((sparse(1:NumInsideNodes, 1:NumInsideNodes, A_old*(1 - dt*ksi) - dt*sum(Du*M_A,2), NumInsideNodes, NumInsideNodes) + dt*Du*M_A)*Uy_local + dt*sum(FluxUy,2))./A;
        case 'ZeroFlux'
            Ux_local = ((sparse(1:NumInsideNodes, 1:NumInsideNodes, A*(1 - dt*ksi) - dt*sum(Du*M_A,2), NumInsideNodes, NumInsideNodes) + dt*Du*M_A)*Ux_local + dt*sum(FluxUx,2))./A;
            Uy_local = ((sparse(1:NumInsideNodes, 1:NumInsideNodes, A*(1 - dt*ksi) - dt*sum(Du*M_A,2), NumInsideNodes, NumInsideNodes) + dt*Du*M_A)*Uy_local + dt*sum(FluxUy,2))./A;
    end
%         Ux_local = sparse( diag((A_old./A),0) + dt*sparse(diag(1./A,0))*MV - dt*diag( sparse(diag(1./A,0))*MV*ones(NumInsideNodes,1),0 ) -dt*ksi)*Ux_local + dt*sum((FluxUx),2);
%         Uy_local = sparse( diag((A_old./A),0) + dt*sparse(diag(1./A,0))*MV - dt*diag( sparse(diag(1./A,0))*MV*ones(NumInsideNodes,1),0 ) -dt*ksi)*Uy_local + dt*sum((FluxUy),2);
    M_local = ((sparse(1:NumInsideNodes, 1:NumInsideNodes, A_old - dt*sum(D*M_A + k*M_B.M, 2), NumInsideNodes, NumInsideNodes) + (D*M_A - k*M_B.M)*dt)*M_local)./A;
   
    Ux(InsideOldMask) = Ux_local;
    Uy(InsideOldMask) = Uy_local;
    M(InsideOldMask) = M_local;
%     sum(M_local.*A)
    
% Adjust concentaration at the nodes neighboring those that disappear and new nodes

    [Ux, Uy, M, Global_Area, InsideOldMask, BoundaryOldMask, NatNeighborPairs] = GoneNodesAdjustment(gone_nodes, Global_Area, Ux, Uy, M, PosX, PosY, InsideOldMask, BoundaryOldMask, NatNeighborPairs, NumNodesX, NumNodesY, MeshSize, Front);
    [Ux, Uy, M, Global_Area, InsideOldMask, NatNeighborPairs] = NewNodesAdjustment(new_nodes, Global_Area, Ux, Uy, M, PosX, PosY, InsideOldMask, BoundaryOldMask, NatNeighborPairs, NumNodesX, NumNodesY, MeshSize, Front);
%     time
% Collect the results    

    if config.returnAllData && ~mod(iT,max(round(NumTimeNodes/NumFrames),1))
        ii = iT/max(round(NumTimeNodes/NumFrames),1);
        Time_All(ii) = time;
        Front_All{ii} = Front;
        FrontVelocity = zeros(size(Front.Polygon));
        for iVertex = 1:size(Front.Polygon,1)
            FrontVelocity(iVertex,:) = FT_velo_func(time, Front.Polygon(iVertex,:));
        end
        FrontVelocity_All{ii} = FrontVelocity;
        Ux_All(:,ii) = Ux;
        Uy_All(:,ii) = Uy;
        M_All(:,ii) = M;
        Global_Area_All(:,iT) = Global_Area;
        
    end
    
% Visualize solution    
    
    if config.Visualization && ~mod(iT,round(NumTimeNodes/100))
        if iT == round(NumTimeNodes/100)
            figure('Position', [100 400 1400 500]);
        end
        subplot(1,2,1)
        VisualizeSolution(M, PosX, PosY, MeshSize, time, Front, 0, 1)
        xlim(Domain{1})
        ylim(Domain{2})
        colorbar
        subplot(1,2,2)
        VisualizeSolution(sqrt(Ux.^2+Uy.^2), PosX, PosY, MeshSize, time, Front, 0, 0)
        colorbar
        hold on
        VisualizeSolution({Ux, Uy}, PosX, PosY, MeshSize, time, Front, 1)
        xlim(Domain{1})
        ylim(Domain{2})    
        hold off
        
%         caxis([0 100]);
        %axis equal
        pause(0.1)
    end
    
    BoundaryOldMask = BoundaryMask;
    InteriorOldMask = InteriorMask;
    A_old = Global_Area(InsideMask);
    if isempty(gone_nodes) && isempty(new_nodes)
        doVoronoi = false;
    else
        doVoronoi = true;
    end
end;

if ~config.returnAllData
    Ux_All = Ux;
    Uy_All = Uy;
    M_All = M;
    Time_All = time;
    Front_All{1} = Front;
    Global_Area_All = Global_Area;
    FrontVelocity_All = zeros(size(Front.Polygon));
    for iVertex = 1:size(Front.Polygon,1)
        FrontVelocity_All(iVertex,:) = FT_velo_func(time, Front.Polygon(iVertex,:));
    end
end

mexFT_Free(f);

