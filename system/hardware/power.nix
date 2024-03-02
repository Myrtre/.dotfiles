{ ... }: 

{
  services.tlp = {
    enable = true;
    settings = {
      CPU_SALING_GOVERNOR_ON_AC = "performance";
      CPU_SALING_GOVERNOR_ON_BAT = "schedutil";
      
      CPU_ENERGY_PERF_POLICY_ON_AC = "power";
      CPU_ENERGY_PERF_POLICY_ON_BAT = "performance";
      
      CPU_MIN_PERF_ON_AC = 0;
      CPU_MAX_PERF_ON_AC = 100;
      CPU_MIN_PERF_ON_BAT = 0;
      CPU_MAX_PERF_ON_BAT = 80;
    };
  };
}