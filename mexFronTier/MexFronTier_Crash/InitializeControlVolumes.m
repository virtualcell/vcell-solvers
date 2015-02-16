function [NatNeighborPairs, NatNeighborBoundaryPairs, Vertices, VoronoiCells, Global_Area, D_Inv, Directions, MidPoints] =...
    InitializeControlVolumes(PosX, PosY, InsideMask, InteriorMask, BoundaryMask, Front, MeshSize, NumNodesX, NumNodesY)

Global_Area = zeros(NumNodesX*NumNodesY,1);
Global_Area(InteriorMask) = MeshSize*MeshSize;

% Calculate metrics for irregular Voronoi cells near the boundary

BoundaryIdx = find(BoundaryMask);
[NatNeighborBoundaryPairs, Vertices, VoronoiCells] = ...
    GetVoronoiDecomposition(BoundaryIdx, PosX, PosY, InsideMask, [], NumNodesX, NumNodesY);
[D_Inv_Boundary, Directions, MidPoints] = GetBoundaryIndependentMetrics(NatNeighborBoundaryPairs, PosX, PosY);

% Fill in numbers for "interior" regular Voronoi cells (squares)

InteriorIdx = find(InteriorMask);
NatNeighborPairs = [InteriorIdx-1 InteriorIdx
    InteriorIdx+1 InteriorIdx
    InteriorIdx-NumNodesX InteriorIdx
    InteriorIdx+NumNodesX InteriorIdx
    NatNeighborBoundaryPairs];
D_Inv = [1/MeshSize*ones(4*length(InteriorIdx),1); D_Inv_Boundary];

[~, Area, ~] =...
    GetBoundaryVolumes(BoundaryIdx, MeshSize, VoronoiCells, Vertices, NatNeighborBoundaryPairs, Directions, MidPoints, Front);
Global_Area(BoundaryMask) = Area;
