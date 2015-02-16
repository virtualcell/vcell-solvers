function strlength = DisplayProgress(ticID, n, N, strlength)

% if ~mod(n,round(N/50))
    elapsedTime = toc(ticID);
    fprintf(repmat('\b',1,strlength));
    str = [num2str(n/N*100) ' percent done... Elapsed time is ' num2str(elapsedTime) ' seconds.'];
    fprintf(str);
    strlength = length(str);
% end
