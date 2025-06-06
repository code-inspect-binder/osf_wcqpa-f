# Executable Environment for OSF Project [wcqpa](https://osf.io/wcqpa/)

This repository was automatically generated as part of a project to test the reproducibility of open science projects hosted on the Open Science Framework (OSF).

**Project Title:** Dataset and R code for: Suicidal risk and impulsivity-related traits among young Argentinean college students during a quarantine of up to 103-days duration: Longitudinal evidence from the COVID-19 pandemic

**Project Description:**
> This study is aimed to examine longitudinal changes on suicidal risk levels, adjusting for impulsivity-related traits, quarantine duration, main demographic factors, mental disorder history, and loneliness, in young Argentinean college students with (ideation; attempt) and without suicidal behavior history, during a quarantine of up to 103 days-duration due to the COVID-19 pandemic. PUBLISHED IN: López Steinmetz L. C., Fong S. B. &amp; Godoy J. C. (2021). Suicidal risk and impulsivity-related traits among young Argentinean college students during a quarantine of up to 103-day duration: Longitudinal evidence from the COVID-19 pandemic. Suicide and Life-Threatening Behavior, 51(6), 1175-1188. https://doi.org/10.1111/sltb.12799 

**Original OSF Page:** [https://osf.io/wcqpa/](https://osf.io/wcqpa/)

---

**Important Note:** The contents of the `wcqpa_src` folder were cloned from the OSF project on **12-03-2025**. Any changes made to the original OSF project after this date will not be reflected in this repository.

The `DESCRIPTION` file was automatically added to make this project Binder-ready. For more information on how R-based OSF projects are containerized, please refer to the `osf-to-binder` GitHub repository: [https://github.com/Code-Inspect/osf-to-binder](https://github.com/Code-Inspect/osf-to-binder)

## flowR Integration

This version of the repository has the **[flowR Addin](https://github.com/flowr-analysis/rstudio-addin-flowr)** preinstalled. flowR allows visual design and execution of data analysis workflows within RStudio, supporting better reproducibility and modular analysis pipelines.

To use flowR, open the project in RStudio and go to `Addins` > `flowR`.

## How to Launch:

**Launch in your Browser:**

🚀 **MyBinder:** [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/code-inspect-binder/osf_wcqpa-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment in your web browser.
   * Please note that Binder may take a few minutes to build the environment.

🚀 **NFDI JupyterHub:** [![NFDI](https://nfdi-jupyter.de/images/nfdi_badge.svg)](https://hub.nfdi-jupyter.de/r2d/gh/code-inspect-binder/osf_wcqpa-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment on the NFDI JupyterHub platform.

**Access Downloaded Data:**
The downloaded data from the OSF project is located in the `wcqpa_src` folder.

## Run via Docker for Long-Term Reproducibility

In addition to launching this project using Binder or NFDI JupyterHub, you can reproduce the environment locally using Docker. This is especially useful for long-term access, offline use, or high-performance computing environments.

### Pull the Docker Image

```bash
docker pull meet261/repo2docker-wcqpa-f:latest
```

### Launch RStudio Server

Run the container (with a name, e.g. `rstudio-dev`):
```bash
docker run -it --name rstudio-dev --platform linux/amd64 -p 8888:8787 --user root meet261/repo2docker-wcqpa-f bash
```

Inside the container, start RStudio Server with no authentication:
```bash
/usr/lib/rstudio-server/bin/rserver --www-port 8787 --auth-none=1
```

Then, open your browser and go to: [http://localhost:8888](http://localhost:8888)

> **Note:** If you're running the container on a remote server (e.g., via SSH), replace `localhost` with your server's IP address.
> For example: `http://<your-server-ip>:8888`

## Looking for the Base Version?

For the original Binder-ready repository **without flowR**, visit:
[osf_wcqpa](https://github.com/code-inspect-binder/osf_wcqpa)

