function [Ux, Uy, M, Global_Area, InsideOldMask, NatNeighborPairs] = NewNodesAdjustment(new_nodes, Global_Area, Ux, Uy, M, PosX, PosY, InsideOldMask, BoundaryOldMask, NatNeighborPairs, NumNodesX, NumNodesY, MeshSize, Front)     

for Idx = new_nodes'
    InsideOldIdx = find(InsideOldMask); 
    [~, Id] = min((PosX(InsideOldMask)-PosX(Idx)).^2+(PosY(InsideOldMask)-PosY(Idx)).^2);
    ClosestIdx = InsideOldIdx(Id);
    NeighborOfClosestIdx = NatNeighborPairs(NatNeighborPairs(:,2)==ClosestIdx,1);
    PotentialNeighbors = [];
    for neighborIdx = NeighborOfClosestIdx'
        PotentialNeighbors = [PotentialNeighbors; NatNeighborPairs(NatNeighborPairs(:,2)==neighborIdx,1)];
    end
    PotentialNeighbors = intersect(unique([ClosestIdx; NeighborOfClosestIdx; PotentialNeighbors]), InsideOldIdx);
    
    [NatNeighbor_ij, Vertices, VoronoiCells] =...
        GetVoronoiDecomposition(Idx, PosX, PosY, InsideOldMask, PotentialNeighbors, NumNodesX, NumNodesY);
    [~, Directions, MidPoints] = GetBoundaryIndependentMetrics(NatNeighbor_ij, PosX, PosY);
    [~, Area_NewNode, ~] =...
        GetBoundaryVolumes(Idx, MeshSize, VoronoiCells, Vertices, NatNeighbor_ij, Directions, MidPoints, Front);
    neighbors = NatNeighbor_ij(:,1);
    InsideOldMask(Idx) = true;
    BoundaryOldMask(Idx) = true;
    NatNeighborPairs = [NatNeighborPairs; NatNeighbor_ij; NatNeighbor_ij(:, [2 1])];

    neighbors_neighbors = [];
    for neighborIdx = neighbors'
        neighbors_neighbors = [neighbors_neighbors; NatNeighborPairs(NatNeighborPairs(:,2)==neighborIdx,1)];
    end
    neighbors_neighbors = intersect(unique(neighbors_neighbors), [InsideOldIdx; Idx]);

    [NatNeighbor_ij, Vertices, VoronoiCells] =...
        GetVoronoiDecomposition(neighbors, PosX, PosY, InsideOldMask, neighbors_neighbors, NumNodesX, NumNodesY);
    [~, Directions, MidPoints] = GetBoundaryIndependentMetrics(NatNeighbor_ij, PosX, PosY);
    [~, Area_Neighbors, ~] =...
        GetBoundaryVolumes(neighbors, MeshSize, VoronoiCells, Vertices, NatNeighbor_ij, Directions, MidPoints, Front);
    
    Area_old_Neighbors = Global_Area(neighbors);
    Global_Area(Idx) = Area_NewNode;
    Global_Area(neighbors) = Area_Neighbors;
    
    Diff_Area = Area_old_Neighbors - Area_Neighbors;
    fi = Diff_Area/Area_NewNode;
    if abs(sum(fi)-1)>1e-12
        disp('Inconsistancy')
        disp(log10(abs(sum(fi)-1)))
    end
    if min(fi)<-1e-12
        disp('Negative fi')
        disp(min(fi))
    end
    Ux(Idx) = sum(Ux(neighbors).*fi);
    Uy(Idx) = sum(Uy(neighbors).*fi);
    M(Idx) = sum(M(neighbors).*fi);
end;