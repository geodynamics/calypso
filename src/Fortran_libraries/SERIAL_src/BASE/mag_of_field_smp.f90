!>@file  mag_of_field_smp.f90
!!      module mag_of_field_smp
!!
!! @author  H. Matsui
!! @date Programmed in June, 2005
!! @n    Modified in Apr., 2013
!!
!>@brief Evaluate amplitude of field or lengh scale of field
!!      Need $omp parallel to use these routines
!!
!!@verbatim
!!      subroutine cal_vector_magnitude(nnod, d_fld, d_mag)
!!        integer(kind = kint), intent(in) :: nnod
!!        real(kind=kreal), intent(in)    :: d_fld(nnod,3)
!!        real(kind=kreal), intent(inout) :: d_mag(nnod)
!!      subroutine cal_sym_tensor_magnitude(nnod, d_fld, d_mag)
!!         integer (kind = kint) :: nnod
!!         real(kind=kreal), intent(in)    :: d_fld(nnod,6)
!!         real(kind=kreal), intent(inout) :: d_mag(nnod)
!!      subroutine cal_asym_tensor_magnitude(nnod, d_fld, d_mag)
!!        integer (kind = kint) :: nnod
!!        real(kind=kreal), intent(in)    :: d_fld(nnod,3)
!!        real(kind=kreal), intent(inout) :: d_mag(nnod)
!!
!!      subroutine cal_len_scale_by_rot_smp(nnod, d_fld, d_rot, d_len)
!!        integer (kind = kint) :: nnod
!!        real(kind=kreal), intent(in)    :: d_fld(nnod,3)
!!        real(kind=kreal), intent(in)    :: d_rot(nnod,3)
!!        real(kind=kreal), intent(inout) :: d_len(nnod)
!!      subroutine cal_len_scale_by_diffuse_smp(nnod, d_fld, d_diffuse, &
!!     &                                    d_len)
!!        integer (kind = kint) :: nnod
!!        real(kind=kreal), intent(in)    :: d_fld(nnod)
!!        real(kind=kreal), intent(in)    :: d_diffuse(nnod)
!!        real(kind=kreal), intent(inout) :: d_len(nnod)
!!@endverbatim
!!
!!@n @param  np_smp   Number of SMP processes
!!@n @param  nnod     Number of data points
!!@n @param  inod_smp_stack(0:np_smp)
!!                    End address of each SMP process
!!
!!@n @param  d_fld(nnod,nd)   Input vector data
!!@n @param  d_rot(nnod,3)    curl of vector data
!!@n @param  d_diffuse(nnod)  diffusion of field data
!!@n @param  d_mag(nnod)      amplitude of vector
!!@n @param  d_len(nnod)      length scale of field
!
      module mag_of_field_smp
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_vector_magnitude(nnod, d_fld, d_mag)
!
       integer(kind = kint), intent(in) :: nnod
       real(kind=kreal), intent(in)    :: d_fld(nnod,3)
       real(kind=kreal), intent(inout) :: d_mag(nnod)
!
       integer (kind = kint) :: inod
!
!$omp do private(inod)
       do inod = 1, nnod
         d_mag(inod) = sqrt( d_fld(inod,1)*d_fld(inod,1)                &
     &                     + d_fld(inod,2)*d_fld(inod,2)                &
     &                     + d_fld(inod,3)*d_fld(inod,3) )
      end do
!$omp end do nowait
!
      end subroutine cal_vector_magnitude
!
! -----------------------------------------------------------------------
!
      subroutine cal_sym_tensor_magnitude(nnod, d_fld, d_mag)
!
       integer (kind = kint) :: nnod
       real(kind=kreal), intent(in)    :: d_fld(nnod,6)
       real(kind=kreal), intent(inout) :: d_mag(nnod)
!
       integer (kind = kint) :: inod
!
!$omp do private(inod)
      do inod = 1, nnod
         d_mag(inod) = sqrt(   d_fld(inod,1)*d_fld(inod,1)              &
     &                   + two*d_fld(inod,2)*d_fld(inod,2)              &
     &                   + two*d_fld(inod,3)*d_fld(inod,3)              &
     &                   +     d_fld(inod,4)*d_fld(inod,4)              &
     &                   + two*d_fld(inod,5)*d_fld(inod,5)              &
     &                   +     d_fld(inod,6)*d_fld(inod,6) )
      end do
!$omp end do nowait
!
      end subroutine cal_sym_tensor_magnitude
!
! -----------------------------------------------------------------------
!
      subroutine cal_asym_tensor_magnitude(nnod, d_fld, d_mag)
!
       integer (kind = kint) :: nnod
       real(kind=kreal), intent(in)    :: d_fld(nnod,3)
       real(kind=kreal), intent(inout) :: d_mag(nnod)
!
       integer (kind = kint) :: inod
!
!$omp do private(inod)
      do inod = 1, nnod
        d_mag(inod) = two*sqrt( d_fld(inod,1)*d_fld(inod,1)             &
     &                        + d_fld(inod,2)*d_fld(inod,2)             &   
     &                        + d_fld(inod,3)*d_fld(inod,3) )
      end do
!$omp end do nowait
!
      end subroutine cal_asym_tensor_magnitude
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_len_scale_by_rot_smp(nnod, d_fld, d_rot, d_len)
!
      integer (kind = kint) :: nnod
      real(kind=kreal), intent(in)    :: d_fld(nnod,3)
      real(kind=kreal), intent(in)    :: d_rot(nnod,3)
      real(kind=kreal), intent(inout) :: d_len(nnod)
!
       integer (kind = kint) :: inod
!
!
!$omp do private(inod)
      do inod = 1, nnod
         d_len(inod) = d_rot(inod,1)**2 + d_rot(inod,2)**2              &
     &               + d_rot(inod,3)**2
      end do
!$omp end do nowait
!$omp do private(inod)
      do inod = 1, nnod
        if(d_len(inod) .eq. zero) then
          d_len(inod) = zero
        else
          d_len(inod) =  (d_fld(inod,1)**2 + d_fld(inod,2)**2           &
     &                  + d_fld(inod,3)**2) / d_len(inod)
          d_len(inod) = sqrt(d_len(inod))
        end if
      end do
!$omp end do nowait
!
      end subroutine cal_len_scale_by_rot_smp
!
!-----------------------------------------------------------------------
!
      subroutine cal_len_scale_by_diffuse_smp(nnod, d_fld, d_diffuse,   &
     &                                        d_len)
!
      integer (kind = kint) :: nnod
      real(kind=kreal), intent(in)    :: d_fld(nnod)
      real(kind=kreal), intent(in)    :: d_diffuse(nnod)
      real(kind=kreal), intent(inout) :: d_len(nnod)
!
      integer (kind = kint) :: inod
!
!
!$omp do private(inod)
      do inod = 1, nnod
        if(d_fld(inod) .eq. zero) then
          d_len(inod) = zero
        else
          d_len(inod) =  abs(d_fld(inod)  / d_diffuse(inod))
          d_len(inod) = sqrt(d_len(inod))
        end if
      end do
!$omp end do nowait
!
      end subroutine cal_len_scale_by_diffuse_smp
!
!-----------------------------------------------------------------------
!
      end module mag_of_field_smp
