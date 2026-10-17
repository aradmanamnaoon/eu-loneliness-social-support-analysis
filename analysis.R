# R replication of the EXECUTED PYTHON analysis (R NOT RUN as of 2026-10-17).
# Requirements: R >= 4.3; install.packages(c('haven','dplyr','sandwich','lmtest','readr'))
# Run: Rscript analysis.R   (in project directory, extract dataset ZIPs into data/ first)
suppressPackageStartupMessages({library(haven);library(dplyr);library(sandwich);library(lmtest);library(readr)})
file4 <- 'data/Data EU4 sample/eu_loneliness_survey_eu4.dta'
file27 <- 'data/Data EU27 sample/eu_loneliness_survey_eu27.dta'
stopifnot(file.exists(file4),file.exists(file27))
EU4 <- as.data.frame(read_dta(file4)); EU27 <- as.data.frame(read_dta(file27))
stopifnot(nrow(EU4)==4029, nrow(EU27)==25646)
prep <- function(d, four_only=FALSE) {
 if(four_only) d <- d[d$country %in% c(10,15,21,27), ,drop=FALSE]
 for(nm in c('loneliness_direct','social_support_b','social_support_c','family_meet_face','friends_meet_face')) {
   mx <- if(nm %in% c('family_meet_face','friends_meet_face')) 7 else 5
   d[[nm]][!(d[[nm]] %in% seq_len(mx))] <- NA
 }
 d$age[!d$age %in% 16:100] <- NA
 d$gender[!d$gender %in% 1:3] <- NA
 d$lonely_some <- ifelse(is.na(d$loneliness_direct),NA,as.integer(d$loneliness_direct<=3))
 d$lonely_often <- ifelse(is.na(d$loneliness_direct),NA,as.integer(d$loneliness_direct<=2))
 d$age10 <- (d$age-50)/10
 d$gender_label <- factor(d$gender, levels=c(1,2,3), labels=c('Man','Woman','Other gender'))
 d$country_label <- factor(d$country)
 d
}
models <- list(EU4=prep(EU4),EU27_same4=prep(EU27,TRUE),EU27_all27=prep(EU27,FALSE))
output <- list()
for(label in names(models)) {
 d <- models[[label]]
 required <- c('lonely_some','social_support_b','friends_meet_face','family_meet_face','age10','gender_label','country_label','w_country')
 fitdf <- d[complete.cases(d[,required]),,drop=FALSE]
 fitdf <- fitdf[fitdf$w_country>0,,drop=FALSE]
 fitdf$gender_label <- relevel(droplevels(fitdf$gender_label),ref='Man')
 # Binomial with noninteger survey weights: quasibinomial avoids misleading warnings.
 fit <- glm(lonely_some ~ social_support_b+friends_meet_face+family_meet_face+age10+gender_label+country_label,
            data=fitdf,family=quasibinomial(link='logit'),weights=w_country)
 se <- sqrt(diag(vcovHC(fit,type='HC3')))
 term <- 'social_support_b'; beta<-coef(fit)[term]; bse<-se[term]
 output[[label]] <- data.frame(sample=label,n=nrow(fitdf),OR=exp(beta),low=exp(beta-1.96*bse),high=exp(beta+1.96*bse))
 cat(label,'n=',nrow(fitdf),'OR=',round(exp(beta),3),'CI=',round(exp(beta-1.96*bse),3),round(exp(beta+1.96*bse),3),'\n')
}
dir.create('output',showWarnings=FALSE)
write_csv(do.call(rbind,output),'output/r_reproduction_main.csv')
writeLines(capture.output(sessionInfo()),'output/R_SESSION_INFO.txt')
# Compare output against Python (expected small covariance-implementation differences).
# Until R is actually run, do not present any of this file's output as observed.
