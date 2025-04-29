@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Student Data Definition'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZRP_R_STUDENT
with parameters
p_student_id : zrp_student_id
as select from zrp_student

association[1..1] to ZRP_R_UNIVERSITY as _University on $projection.UniversityId = _University.UniversityId
   {
key zrp_student.student_id as StudentId,  

    zrp_student.university_id as UniversityId,
    zrp_student.student_name as StudentName,
    zrp_student.student_age as StudentAge,
    zrp_student.student_major as StudentMajor,
    zrp_student.student_email as StudentEmail,
    _University   
}
where zrp_student.student_id =$parameters.p_student_id
