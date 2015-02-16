function CreateMovie(M_All, Ux_All, Uy_All, PosX, PosY, Time_All, Front_All, FileName, Domain, MeshSize)

% h = figure('Position', [100 400 1400 500]);
h = figure('Position', [100 400 1700 230]);
M = VideoWriter([FileName '.avi']);
M.FrameRate = 15; 
MLimits = [min(min(M_All)) max(max(M_All))];
ULimits = [min(min(sqrt(Ux_All.^2+Uy_All.^2))) max(max(sqrt(Ux_All.^2+Uy_All.^2)))];
open(M);
skip = max(floor(length(Time_All)/8/M.FrameRate),1);
for iT = 1:skip:length(Time_All)
    subplot(1,2,1)
    VisualizeSolution(M_All(:,iT), PosX, PosY, MeshSize, Time_All(iT), Front_All{iT}, 0, 0)
    text(Domain{1}(1)+0.5, Domain{2}(2)*0.9, ['t = ' num2str(Time_All(iT),2)], 'FontSize', 14,  'Color', 'k')
    xlim(Domain{1})
    ylim(Domain{2})
    title('Myosin', 'FontSize', 14)
    if iT == 1
        caxis(MLimits)
        colorbar
    end
    subplot(1,2,2)
    VisualizeSolution(sqrt(Ux_All(:,iT).^2+Uy_All(:,iT).^2), PosX, PosY, MeshSize, Time_All(iT), Front_All{iT}, 0, 0)
    if iT == 1
        caxis(ULimits)
        colorbar
    end
    hold on
%     VisualizeSolution({Ux_All(:,iT), Uy_All(:,iT)}, PosX, PosY, Time_All(iT), Front_All{iT}, 1)
    xlim(Domain{1})
    ylim(Domain{2})
    title('Velocity magnitude', 'FontSize', 14)
    hold off
    frame = getframe(h);
    writeVideo(M,frame);
end
close(M)