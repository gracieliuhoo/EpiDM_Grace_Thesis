% Overall Hierarchical Meta-d'
% Load and Clean Data
% Load and Clean Data
nR_S1 = load('/Users/jcast/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Gaston/nR_S1_50.mat')
nR_S1 = nR_S1(1).x
nR_S1 = mat2cell(nR_S1,ones(1,50),[6])
nR_S2 = load('/Users/jcast/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Gaston/nR_S2_50.mat')
nR_S2 = nR_S2(1).x
nR_S2 = mat2cell(nR_S2,ones(1,50),[6])

% Run model of Hierarchical Meta-d'
hierarchical_metamem = fit_meta_d_mcmc_group(nR_S1, nR_S2)

% Evaluate MCMC
%R hat values (Chain convergence)
hierarchical_metamem.mcmc.Rhat(1)
hierarchical_metamem.mcmc.Rhat(1).c1 > 1.01
hierarchical_metamem.mcmc.Rhat(1).d1 > 1.01
hierarchical_metamem.mcmc.Rhat(1).Mratio > 1.01
%MCMC samples (Chain Mixing/No drift)
plotSamples(exp(hierarchical_metamem.mcmc.samples.mu_logMratio))
plotSamples(exp(hierarchical_metamem.mcmc.samples.sigma_logMratio))
plotSamples(hierarchical_metamem.mcmc.samples.mu_c2)
plotSamples(hierarchical_metamem.mcmc.samples.sigma_c2)
plotSamples(hierarchical_metamem.mcmc.samples.deviance)
%Deviance
hierarchical_metamem.mcmc.dic %360.47

% ANALYSIS
disp("d1")
median(hierarchical_metamem.d1) %Average d'; (0.44)
std(hierarchical_metamem.d1(:)) %Standard deviation d'; (0.34)
calc_HDI(hierarchical_metamem.d1(:)) % -0.19 - 1.00
calc_HDI(hierarchical_metamem.d1(:), .89) % -0.091 - 0.96
disp("c1")
median(hierarchical_metamem.c1) %Average c'; (0) liberal responds yes more than an ideal observer would
std(hierarchical_metamem.c1(:)) %Standard deviation d'; (0.095)
calc_HDI(hierarchical_metamem.c1(:)) % -0.15 - 0.19
calc_HDI(hierarchical_metamem.c1(:), .89) % -0.15 - 0.14
disp("meta-d'")
median(hierarchical_metamem.meta_d) %Average meta-d'; (0.41)
std(hierarchical_metamem.meta_d(:)) %Standard deviation d'; (0.29)
calc_HDI(hierarchical_metamem.meta_d(:)) % -0.16 - 0.96
calc_HDI(hierarchical_metamem.meta_d(:), .89) % -0.08 - 0.84
disp("Mratio")
median(hierarchical_metamem.Mratio) %Average meta-d'/d'; (0.89)
std(hierarchical_metamem.Mratio(:)) %Standard deviation d'; (0.12)
calc_HDI(hierarchical_metamem.Mratio(:)) % 0.71 - 1.18
calc_HDI(hierarchical_metamem.Mratio(:), .89) % 0.71 - 1.09
