function [Ux, Uy, M, Global_Area, InsideOldMask, BoundaryOldMask, NatNeighborPairs] = GoneNodesAdjustment(gone_nodes, Global_Area, Ux, Uy, M, PosX, PosY, InsideOldMask, BoundaryOldMask, NatNeighborPairs, NumNodesX, NumNodesY, MeshSize, Front)     

outIdx = [];
for Idx = gone_nodes'
    outIdx = [outIdx; Idx]; 
    old_neighbors = setdiff(unique(NatNeighborPairs(NatNeighborPairs(:,2)==Idx,1)), outIdx);
    old_neighbors_neighbors = [];
    for neighborIdx = old_neighbors'
        old_neighbors_neighbors = [old_neighbors_neighbors; NatNeighborPairs(NatNeighborPairs(:,2)==neighborIdx,1)];
    end
    old_neighbors_neighbors = setdiff(unique(old_neighbors_neighbors), outIdx);
        
    InsideOldMask(Idx) = false;
    BoundaryOldMask(Idx) = false;
    
    [NatNeighbor_ij, Vertices, VoronoiCells] =...
        GetVoronoiDecomposition(old_neighbors, PosX, PosY, InsideOldMask, old_neighbors_neighbors, NumNodesX, NumNodesY);
    [~, Directions, MidPoints] = GetBoundaryIndependentMetrics(NatNeighbor_ij, PosX, PosY);
    [~, Area_Neighbors, ~] =...
        GetBoundaryVolumes(old_neighbors, MeshSize, VoronoiCells, Vertices, NatNeighbor_ij, Directions, MidPoints, Front);
    
    NatNeighborPairs = [NatNeighborPairs; NatNeighbor_ij; NatNeighbor_ij(:, [2 1])];
    
    Area_old_GoneNode = Global_Area(Idx);
    Area_old_Neighbors = Global_Area(old_neighbors);
    Global_Area(old_neighbors) = Area_Neighbors;
    Global_Area(Idx) = 0;

    Diff_Area = Area_Neighbors - Area_old_Neighbors;
    fractions = abs(Diff_Area)/sum(abs(Diff_Area));
    Ux(old_neighbors) = (Ux(old_neighbors).*Area_old_Neighbors+...
        Area_old_GoneNode*Ux(Idx).*fractions)./Area_Neighbors;
    Ux(Idx)=0;
    Uy(old_neighbors) = (Uy(old_neighbors).*Area_old_Neighbors+...
        Area_old_GoneNode*Uy(Idx).*fractions)./Area_Neighbors;
    Uy(Idx)=0; 
    M(old_neighbors) = (M(old_neighbors).*Area_old_Neighbors+...
        Area_old_GoneNode*M(Idx).*fractions)./Area_Neighbors;
    M(Idx)=0; 
end;