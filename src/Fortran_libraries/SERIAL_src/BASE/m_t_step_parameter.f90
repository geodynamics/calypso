!
!      module  m_t_step_parameter
!.......................................................................
!
      module  m_t_step_parameter
!
!
      use m_precision
!
      implicit  none
!
      real(kind=kreal) :: time
      real(kind=kreal) :: time_init
! 
      integer(kind=kint) :: i_step_MHD
      integer(kind=kint) :: istep_max_dt
      integer(kind=kint) :: istep_flex_to_max = 0
! 
      real(kind=kreal)   :: elapsed_time
! 
      integer(kind= kint) :: iflag_flex_step_changed = 0
      integer(kind= kint) :: i_interval_flex_2_max
!
      integer(kind=kint) :: iflag_starting =     0
      integer(kind=kint) :: iflag_initial_step = 0
      integer(kind=kint) :: iflag_SGS_initial =  1
!
      integer(kind=kint) :: i_step_init
      integer(kind=kint) :: i_step_number
! 
      integer(kind=kint) :: istep_rst_start
      integer(kind=kint) :: istep_rst_end
! 
      integer(kind=kint) :: i_step_check
      real(kind=kreal)   :: delta_t_step_check
! 
      integer(kind=kint) :: i_step_output_rst = 0
      integer(kind=kint) :: i_step_output_ucd = 0
      integer(kind=kint) :: i_step_output_sph = 0
      real(kind=kreal)   :: delta_t_output_rst
      real(kind=kreal)   :: delta_t_output_ucd
      real(kind=kreal)   :: delta_t_output_sph
!
      integer(kind=kint) :: i_step_output_pvr
      integer(kind=kint) :: i_step_output_psf
      integer(kind=kint) :: i_step_output_iso
      integer(kind=kint) :: i_step_output_fline
      real(kind=kreal)   :: delta_t_output_pvr
      real(kind=kreal)   :: delta_t_output_psf
      real(kind=kreal)   :: delta_t_output_iso
      real(kind=kreal)   :: delta_t_output_fline
! 
      integer(kind=kint) :: i_step_output_monitor
      real(kind=kreal)   :: delta_t_output_monitor
! 
      integer(kind=kint) :: i_step_output_boundary
      real(kind=kreal)   :: delta_t_output_boundary
! 
      integer(kind=kint) :: ucd_step
! 
      integer(kind=kint) :: i_step_sgs_output
      real(kind=kreal)   :: delta_t_sgs_output
      integer(kind=kint) :: i_step_sgs_coefs
!
      end module  m_t_step_parameter
