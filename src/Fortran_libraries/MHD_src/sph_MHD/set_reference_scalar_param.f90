!>@file   set_reference_scalar_param.f90
!!@brief  module set_reference_scalar_param
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed by H. Okuda in 2000
!!@n    Mmodified by H. Matsui in 2001
!!@n    Mmodified by H. Matsui in Aug., 2007
!!@n    Mmodified by H. Matsui in Jan, 2017
!
!> @brief set reference fields for MHD simulation from control data
!!
!!@verbatim
!!      subroutine set_reference_scalar_ctl                             &
!!     &        (charaflag, ref_ctl, ref_param, takepiro)
!!        type(read_character_item), intent(in) :: ref_temp_ctl
!!        type(read_character_item), intent(in) :: stratified_ctl
!!        type(reference_point_control), intent(in) :: low_temp_ctl
!!        type(reference_point_control), intent(in) :: high_temp_ctl
!!        type(reference_point_control), intent(in) :: takepiro_ctl
!!        type(reference_scalar_param), intent(inout) :: ref_param
!!        type(takepiro_model_param), intent(inout) :: takepiro
!!@endverbatim
!
      module set_reference_scalar_param
!
      use m_precision
      use m_error_IDs
!
      use m_machine_parameter
!
      use t_reference_scalar_param
      use t_ctl_data_temp_model
      use t_file_IO_parameter
!
      implicit  none
!
      private :: set_linear_ref_scalar_ctl, set_takepiro_scalar_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_reference_scalar_ctl                               &
     &        (charaflag, ref_ctl, ref_param, takepiro)
!
      use calypso_mpi
      use t_ctl_data_temp_model
!
      character(len = kchara), intent(in) :: charaflag
      type(reference_temperature_ctl), intent(in) :: ref_ctl
!
      type(reference_scalar_param), intent(inout) :: ref_param
      type(takepiro_model_param), intent(inout) :: takepiro
!
!
      if(iflag_debug .ge. iflag_routine_msg) write(*,*) trim(charaflag)
!z
      call set_linear_ref_scalar_ctl                                    &
     &   (ref_ctl%reference_ctl, ref_ctl%ref_file_ctl,                  &
     &    ref_ctl%low_ctl, ref_ctl%high_ctl, ref_param)
      call set_takepiro_scalar_ctl                                      &
     &   (ref_ctl%stratified_ctl, ref_ctl%takepiro_ctl,                 &
     &    ref_param%iflag_reference, takepiro)
!
      end subroutine set_reference_scalar_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_linear_ref_scalar_ctl(ref_temp_ctl, ref_file_ctl,  &
     &          low_temp_ctl, high_temp_ctl, ref_param)
!
      use calypso_mpi
      use t_control_array_character
      use m_file_format_switch
      use m_error_IDs
      use delete_data_files
!
      type(read_character_item), intent(in) :: ref_temp_ctl
      type(read_character_item), intent(in) :: ref_file_ctl
      type(reference_point_control), intent(in) :: low_temp_ctl
      type(reference_point_control), intent(in) :: high_temp_ctl
!
      type(reference_scalar_param), intent(inout) :: ref_param
!
      integer (kind = kint) :: iflag
      character(len=kchara) :: tmpchara
!
!   set control for temperature 
!
      if (ref_temp_ctl%iflag .eq. 0) then
        ref_param%iflag_reference = id_no_ref_temp
      else
        tmpchara = ref_temp_ctl%charavalue
        if (cmp_no_case(tmpchara, label_sph_shell)) then
          ref_param%iflag_reference = id_sphere_ref_temp
        else if (cmp_no_case(tmpchara, label_takepiro)) then
          ref_param%iflag_reference = id_takepiro_temp
        else if (cmp_no_case(tmpchara, label_sph_const_heat)) then
          ref_param%iflag_reference = id_linear_r_ref_temp
        else if (cmp_no_case(tmpchara, label_linear_x)) then
          ref_param%iflag_reference = id_x_ref_temp
        else if (cmp_no_case(tmpchara, label_linear_y)) then
          ref_param%iflag_reference = id_y_ref_temp
        else if (cmp_no_case(tmpchara, label_linear_z)) then
          ref_param%iflag_reference = id_z_ref_temp
        else if (cmp_no_case(tmpchara, label_get_numerical)) then
          ref_param%iflag_reference = id_numerical_solution
        else if (cmp_no_case(tmpchara, label_load_file)) then
          ref_param%iflag_reference = id_read_file
        end if
      end if
!
      ref_param%flag_ref_field = .FALSE.
      if     (ref_param%iflag_reference .eq. id_sphere_ref_temp         &
     &   .or. ref_param%iflag_reference .eq. id_takepiro_temp           &
     &   .or. ref_param%iflag_reference .eq. id_numerical_solution      &
     &   .or. ref_param%iflag_reference .eq. id_read_file               &
     &   ) ref_param%flag_ref_field = .TRUE.
!
      if(ref_param%iflag_reference .eq. id_read_file) then
        ref_param%ref_file_IO%iflag_IO = 0
        ref_param%ref_file_IO%iflag_format = id_ascii_file_fmt
        if(ref_file_ctl%iflag .le. 0) then
          call calypso_mpi_abort(ierr_file, 'Set reference field file')
        else
          ref_param%ref_file_IO%file_prefix = ref_file_ctl%charavalue
!
          if(check_file_exist(ref_param%ref_file_IO%file_prefix)        &
     &                                            .eqv. .FALSE.) then
            write(e_message,*) 'File ',                                 &
     &               ref_param%ref_file_IO%file_prefix, ' is missing.'
            call calypso_mpi_abort(ierr_file, e_message)
          end if
        end if
      end if
!
      iflag = low_temp_ctl%depth%iflag*low_temp_ctl%value%iflag
      if (iflag .eq. 0) then
        if(   ref_param%iflag_reference .eq. id_no_ref_temp             &
     &   .or. ref_param%iflag_reference .eq. id_numerical_solution      &
     &     ) then
          ref_param%low_value  =  0.0d0
          ref_param%depth_top  =  0.0d0
        else
          e_message                                                     &
     &          = 'Set lower temperature and its position'
          call calypso_MPI_abort(ierr_fld, e_message)
        end if
      else
        ref_param%low_value  = low_temp_ctl%value%realvalue
        ref_param%depth_top  = low_temp_ctl%depth%realvalue
      end if
!
      iflag = high_temp_ctl%depth%iflag*high_temp_ctl%value%iflag
      if (iflag .eq. 0) then
        if(   ref_param%iflag_reference .eq. id_no_ref_temp             &
     &   .or. ref_param%iflag_reference .eq. id_numerical_solution      &
     &     ) then
          ref_param%high_value =  0.0d0
          ref_param%depth_bottom =  0.0d0
        else
          e_message                                                     &
     &         = 'Set lower temperature and its position'
          call calypso_MPI_abort(ierr_fld, e_message)
        end if
      else
        ref_param%high_value = high_temp_ctl%value%realvalue
        ref_param%depth_bottom = high_temp_ctl%depth%realvalue
      end if
!
      if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'iflag_reference ', ref_param%iflag_reference
        write(*,*) 'low_value ',       ref_param%low_value
        write(*,*) 'high_value ',      ref_param%high_value
        write(*,*) 'depth_top ',       ref_param%depth_top
        write(*,*) 'depth_bottom ',    ref_param%depth_bottom
      end if
!
      end subroutine set_linear_ref_scalar_ctl
!
! -----------------------------------------------------------------------
!
      subroutine set_takepiro_scalar_ctl                                &
     &         (stratified_ctl, takepiro_ctl, iflag_ref, takepiro)
!
      use calypso_mpi
      use t_ctl_data_temp_model
      use t_ctl_data_stratified_model
      use t_control_array_character
!
      type(read_character_item), intent(in) :: stratified_ctl
      type(takepiro_model_control), intent(in) :: takepiro_ctl
!
      integer(kind = kint), intent(inout) :: iflag_ref
      type(takepiro_model_param), intent(inout) :: takepiro
!
      integer (kind = kint) :: iflag
!
!      set control for Takepiro model
!
      if (stratified_ctl%iflag .gt. id_turn_OFF                         &
        .and. yes_flag(stratified_ctl%charavalue))  then
         iflag_ref = id_takepiro_temp
      end if
!
      if (iflag_ref .eq. id_takepiro_temp) then
        iflag = takepiro_ctl%stratified_sigma_ctl%iflag                 &
     &         *takepiro_ctl%stratified_width_ctl%iflag                 &
     &         *takepiro_ctl%stratified_outer_r_ctl%iflag
        if(iflag .eq. 0) then
          e_message                                                     &
     &        = 'Set parameteres for stratification'
          call calypso_MPI_abort(ierr_fld, e_message)
        else
          takepiro%stratified_sigma                                     &
     &          = takepiro_ctl%stratified_sigma_ctl%realvalue
          takepiro%stratified_width                                     &
     &          = takepiro_ctl%stratified_width_ctl%realvalue
          takepiro%stratified_outer_r                                   &
     &           = takepiro_ctl%stratified_outer_r_ctl%realvalue
        end if
      else
        takepiro%stratified_sigma = 0.0d0
        takepiro%stratified_width = 0.0d0
        takepiro%stratified_outer_r = 0.0d0
      end if
!
      if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'iflag_stratified ',   iflag_ref
        write(*,*) 'stratified_sigma ',   takepiro%stratified_sigma
        write(*,*) 'stratified_width ',   takepiro%stratified_width
        write(*,*) 'stratified_outer_r ', takepiro%stratified_outer_r
      end if
!
      end subroutine set_takepiro_scalar_ctl
!
! -----------------------------------------------------------------------
!
      end module set_reference_scalar_param
