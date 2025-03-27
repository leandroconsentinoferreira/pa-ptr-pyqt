function [pressao_dose_nova, flags] = geraFlags(pressao_dose)
%UNTITLED2 Summary of this function goes here
%   Detailed explanation goes here
% flags=[];
% for i=1:length(pressao_dose);
%     if pressao_dose(i)<10;
%         %Considera um ponto preocupante se a pressão medida é inferior a
%         %10mmHg
%         flags(end+1)=i;
%     elseif pressao_dose(i)>200;
%         %Considera um ponto preocupante se a pressão medida é superior a
%         %200mmHg
%         flags(end+1)=i;
%     end;
% end;
% flags

flags = [];
pressao_dose_nova = [];
passo = floor(2*( (length(pressao_dose)^(1/3) )));

% for j=1:passo:length(pressao_dose)
for j=1:1:2 %length(pressao_dose):pressao_dose(end)
    pressao_relativa = [];
    pressao_relativa_sem_flags = [];
    
    try
        pressao_relativa = pressao_dose; %(j:j+passo-1);
    catch
        pressao_relativa = pressao_dose; %(j:length(pressao_dose));
    end
    
    % Mediana ou segundo quartil
    Q2 = median(pressao_relativa);

    % Primeiro quartil 
    Q1 = median(pressao_relativa(pressao_relativa<Q2));

    % Terceiro quartil 
    Q3 = median(pressao_relativa(pressao_relativa>Q2));

    % IQR
    IQR = Q3 - Q1;

    % Limites pelo método de Tukey (Q1 - 1.5xIQR) a (Q3-1.5xIQR) 
    % Alterando para 3*IQR > Extreme Outlier
    Lim_inf = Q1-1.5*IQR;
    Lim_sup = Q3+1.5*IQR;
    
    disp([Lim_inf Lim_sup])

    if (Lim_inf>5) && (Lim_sup<200)
        flags_relativa=sort(find( (pressao_relativa<Lim_inf) | (pressao_relativa>Lim_sup)));
    elseif Lim_inf<=5
        flags_relativa=sort(find(pressao_relativa<5 | pressao_relativa>Lim_sup));
    elseif Lim_sup>=200
        flags_relativa=sort(find(pressao_relativa<Lim_inf | pressao_relativa>200));
    else
        flags_relativa=sort(find(pressao_relativa<5 | pressao_relativa>200));
    end

    flags_abs = flags_relativa + (j-1);
    flags = [flags;flags_abs];
    
    x = ismember(1:numel(pressao_relativa),flags_relativa);
    pressao_relativa_sem_flags = pressao_relativa(~x);
    pressao_media_trecho = mean(pressao_relativa_sem_flags);
    pressao_relativa(flags_relativa) = pressao_media_trecho;
    
    pressao_dose_nova = pressao_relativa; % [pressao_dose_nova; pressao_relativa];
end



end

