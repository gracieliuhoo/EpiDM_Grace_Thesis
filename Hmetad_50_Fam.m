mcmc_params.response_conditional = 0; % Do we want to fit response-conditional meta-d'?
mcmc_params.nchains = 4; % How Many Chains?
mcmc_params.nburnin = 1000; % How Many Burn-in Samples?
mcmc_params.nsamples = 11000;  %How Many Recorded Samples?
mcmc_params.nthin = 1; % How Often is a Sample Recorded?
mcmc_params.doparallel = 0; % Parallel Option
mcmc_params.dic = 1;  % Save DIC

% Dependent groups (Neg/Neut) Hierarchical Meta-d'
% Load and Clean Data
nR_S1_1 = load('/Users/jcast/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Gaston/nR_S1_50_1ON2.mat')
nR_S1_1 = nR_S1_1(1).x
nR_S1_1 = mat2cell(nR_S1_1,ones(1,50),[6,6])
nR_S1_1 = num2cell(nR_S1_1, 1)
nR_S1_1 = cell2struct(nR_S1_1, 'counts', 1)

nR_S2_1 = load('/Users/jcast/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Gaston/nR_S2_50_2ON2.mat')
nR_S2_1 = nR_S2_1(1).x
nR_S2_1 = mat2cell(nR_S2_1,ones(1,50),[6,6])
nR_S2_1 = num2cell(nR_S2_1, 1)
nR_S2_1 = cell2struct(nR_S2_1, 'counts', 1)

% Run Dependent groups model of Hierarchical Meta-d'
hierarchical_metamem_Val = fit_meta_d_mcmc_groupCorr(nR_S1_1, nR_S2_1, mcmc_params)

% Evaluate MCMC
%R hat values (Chain convergence)
hierarchical_metamem_Val.mcmc.Rhat(1).deviance > 1.1
hierarchical_metamem_Val.mcmc.Rhat(1).rho > 1.1
hierarchical_metamem_Val.mcmc.Rhat(1).c1 > 1.1
hierarchical_metamem_Val.mcmc.Rhat(1).d1 > 1.1
hierarchical_metamem_Val.mcmc.Rhat(1).Mratio > 1.1

%MCMC samples (Chain Mixing/No drift)
plotSamples(hierarchical_metamem_Val.mcmc.samples.rho)
plotSamples(hierarchical_metamem_Val.mcmc.samples.deviance)

%Deviance
hierarchical_metamem_Val.mcmc.dic % 3.18 x 10 ^ 3


% ANALYSIS
% Compute HDI of difference in log-ratio
sampleDiff = exp(hierarchical_metamem_Val.mcmc.samples.mu_logMratio(:,:,1)) - exp(hierarchical_metamem_Val.mcmc.samples.mu_logMratio(:,:,2));
hdi = calc_HDI(sampleDiff(:), .89);
fprintf(('\n Mratio session values = %.2f and %.2f'), exp(hierarchical_metamem_Val.mu_logMratio(1)), exp(hierarchical_metamem_Val.mu_logMratio(2))); % 0.72 [Old], 0.61 [New]
fprintf(['\n Estimated difference in Mratio between sessions: ', num2str(exp(hierarchical_metamem_Val.mu_logMratio(1)) - exp(hierarchical_metamem_Val.mu_logMratio(2)))]) % 0.12
fprintf(['\n HDI on difference in log(Mratio): ', num2str(hdi) '\n\n']) % -0.2, 0.43

%Plot HDI difference in log-ratio units
plotSamples(sampleDiff)

%Calculate Correlation
disp("rho")
hierarchical_metamem_Val.rho % 0.18

% d'
disp("d1_Old")
median(hierarchical_metamem_Val.d1(:,1)) %Average d'; (0.58)
disp("HDI_d1_Old")
calc_HDI(hierarchical_metamem_Val.d1(:,1), .89) % -0.056; 1.26
disp("CI_d1_Old")
calc_CI(hierarchical_metamem_Val.d1(:,1)) % -0.059; 1.54
prctile(hierarchical_metamem_Val.d1(:,1), [5.5, 94.5]) %89% CrI (-0.049, 1.35)

disp("d1_New")
median(hierarchical_metamem_Val.d1(:,2)) %Average d'; (0.3)
disp("HDI_d1_New")
calc_HDI(hierarchical_metamem_Val.d1(:,2), .89) % -0.18; 0.81
disp("CI_d1_New")
calc_CI(hierarchical_metamem_Val.d1(:,2)) % -0.26; 1
prctile(hierarchical_metamem_Val.d1(:,2), [5.5, 94.5]) %89% CrI (-0.18, 0.8)

% c (bias)
disp("c1_Old")
median(hierarchical_metamem_Val.c1(:,1)) %Average c'; (0.067) conservative responds 'yes' less than an ideal observer would
disp("HDI_c1_Old")
calc_HDI(hierarchical_metamem_Val.c1(:,1), .89) % -0.15; 0.31
disp("CI_c1_Old")
calc_CI(hierarchical_metamem_Val.c1(:,1)) % -0.3; 0.31

disp("c1_New")
median(hierarchical_metamem_Val.c1(:,2)) %Average c'; (-0.059) conservative bias responds yes less than an ideal observer would
disp("HDI_c1_New")
calc_HDI(hierarchical_metamem_Val.c1(:,2), .89) % -0.22; 0.16
disp("CI_c1_New")
calc_CI(hierarchical_metamem_Val.c1(:,2)) % -0.27; 0.22
Val_CI_diff =hierarchical_metamem_Val.mcmc.samples.c1(:,:,1) - hierarchical_metamem_Val.mcmc.samples.c1(:,:,2)
hdi_C1diff = calc_HDI(Val_CI_diff(:)) %0.032

%M ratio
disp("Mratio_Old")
median(hierarchical_metamem_Val.Mratio(:,1)) %Average meta-d'/d'; (0.75)
std(hierarchical_metamem_Val.Mratio(:,1)) %0.034
calc_HDI(hierarchical_metamem_Val.Mratio(:,1), .89) % 0.69; 0.8
prctile(hierarchical_metamem_Val.Mratio(:,1), [5.5, 94.5]) %89% CrI (0.69, 0.8)


disp("Mratio_New")
median(hierarchical_metamem_Val.Mratio(:,2)) %Average meta-d'/d'; (0.77)
std(hierarchical_metamem_Val.Mratio(:,2)) % 0.29
calc_HDI(hierarchical_metamem_Val.Mratio(:,2)) % 0.39; 1.7
prctile(hierarchical_metamem_Val.Mratio(:,2), [5.5, 94.5]) %89% CrI (0.42, 1.28)


sampleDiff = exp(hierarchical_metamem_Val.mcmc.samples.mu_logMratio(:,:,1)) - exp(hierarchical_metamem_Val.mcmc.samples.mu_logMratio(:,:,2)) 
median(sampleDiff(:)) %Average meta-d'/d'; (0.1)
std(sampleDiff(:)) % 0.19
hdi = calc_HDI(sampleDiff(:), .89)% -0.2, 0.43



