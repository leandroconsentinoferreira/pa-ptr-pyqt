function [inicio_intra,fim_intra,inicio_inter,fim_inter]=identificaPerturba(tempo_dose,x)
%Permite o usuário selecionar o ponto de início da primeira
%perturbação. Com isso, considera que cada perturbação tem duração de 5
%segundos e cada interperturbação de 6 e pega o meio de cada um desses
%intervalos.

    inicio_intra=[];
    inicio_inter=[];
    fim_intra=[];
    fim_inter=[];

    for i=1:15
        if x+9<tempo_dose(end)
            intra=find(tempo_dose>x+1.5 & tempo_dose<x+3.5);
            inter=find(tempo_dose>x+7 & tempo_dose<x+9);
            inicio_intra(end+1)=intra(1);
            fim_intra(end+1)=intra(end);
            inicio_inter(end+1)=inter(1);
            fim_inter(end+1)=inter(end);
            x=x+11;
        else
            i=15;
        end
    end

% subplot(3,1,3)
% plot(tempo_dose,pressao_dose_nova,tempo_dose(inicio_inter),pressao_dose_nova(inicio_inter),'og',tempo_dose(fim_inter),pressao_dose_nova(fim_inter),'xg',tempo_dose(inicio_intra),pressao_dose_nova(inicio_intra),'or',tempo_dose(fim_intra),pressao_dose_nova(fim_intra),'xr'),grid

end

% resposta = menu('A seleção dos intervalos está correta?','Sim','Não');
% 
% if resposta==2
%     
%     inicio_intra=[];
%     inicio_inter=[];
%     fim_intra=[];
%     fim_inter=[];
% 
%     subplot(3,1,3)
%     plot(tempo_dose,pressao_dose_nova),grid
% 
%     parar=1;
%     n=1;
%     msgbox('Selecione manualmente o início de cada perturbação','Modo manual','modal')
%     while parar==1
%         [x,y,botao]= ginput(1);
%         intra=find(tempo_dose>x+0.5 & tempo_dose<x+2.5);
%         inicio_intra(n)=intra(1);
%         fim_intra(n)=intra(end);
%         n=n+1;
%         if botao==3
%             parar=0;
%         end
%     end
%     parar=1;
%     n=1;
%     msgbox('Selecione manualmente o início das interperturbações','Modo manual','modal')
%     while parar==1
%         [x,y,botao]= ginput(1);
%         inter=find(tempo_dose>x+0.5 & tempo_dose<x+2.5);
%         inicio_inter(n)=inter(1);
%         fim_inter(n)=inter(end);
%         n=n+1;
%         if botao==3
%             parar=0;
%         end
%     end
% end