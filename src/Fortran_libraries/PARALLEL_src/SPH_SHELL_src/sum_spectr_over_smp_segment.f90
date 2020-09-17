!>@file   sum_spectr_over_smp_segment.f90
!!@brief  module sum_spectr_over_smp_segment
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform
!!       (Blocked loop version)
!!
!!@verbatim
!!      subroutine sum_spectr_over_smp_in                             &
!!     &         (nkrs, nkrt, n_jk_e, n_jk_o, Smat)
!!      subroutine sum_spectr_over_kr_in                              &
!!     &         (nkrs, nkrt, n_jk_e, n_jk_o, Smat)
!!      subroutine sum_spectr_over_smp_out                            &
!!     &         (nkrs, nkrt, n_jk_e, n_jk_o, Smat)
!!        type(spectr_matrix_omp), intent(inout) :: Smat(np_smp)
!!@endverbatim
!!
      module sum_spectr_over_smp_segment
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_field_matrices_4_legendre
!
      implicit none
!
      integer, external :: omp_get_max_threads
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sum_spectr_over_smp_in                                 &
     &         (nkrs, nkrt, n_jk_e, n_jk_o, Smat)
!
      integer(kind = kint), intent(in) :: nkrs, nkrt
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o
      type(spectr_matrix_omp), intent(inout) :: Smat(np_smp)
!
      integer(kind = kint) :: ip, j
!
!
!$omp parallel
!$omp do private(j,ip)
        do j = 1, nkrs*n_jk_e
          do ip = 2, np_smp
            Smat(1)%pol_e(j) = Smat(1)%pol_e(j) + Smat(ip)%pol_e(j)
          end do
        end do
!$omp end do nowait
!$omp do private(j,ip)
        do j = 1, nkrt*n_jk_e
          do ip = 2, np_smp
            Smat(1)%pol_e(j) = Smat(1)%pol_e(j) + Smat(ip)%pol_e(j)
          end do
        end do
!$omp end do nowait
!$omp do private(j,ip)
        do j = 1, nkrs*n_jk_o
          do ip = 2, np_smp
            Smat(1)%pol_o(j) = Smat(1)%pol_o(j) + Smat(ip)%pol_o(j)
          end do
        end do
!$omp end do nowait
!$omp do private(j,ip)
        do j = 1, nkrt*n_jk_o
          do ip = 2, np_smp
            Smat(1)%tor_o(j) =  Smat(1)%tor_o(j) + Smat(ip)%tor_o(j)
          end do
        end do
!$omp end do
!$omp end parallel
!
      end subroutine sum_spectr_over_smp_in
!
! -----------------------------------------------------------------------
!
      subroutine sum_spectr_over_kr_in                                  &
     &         (nkrs, nkrt, n_jk_e, n_jk_o, Smat)
!
      integer(kind = kint), intent(in) :: nkrs, nkrt
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o
      type(spectr_matrix_omp), intent(inout) :: Smat(np_smp)
!
      integer(kind = kint) :: ip, nr, lt, j
!
!
!$omp parallel
!$omp do private(lt,nr,j,ip)
      do lt = 1, n_jk_e
        do ip = 2, np_smp
          do nr = 1, nkrs
            j = nr + (lt-1)*nkrs
            Smat(1)%pol_e(j) = Smat(1)%pol_e(j) + Smat(ip)%pol_e(j)
          end do
        end do
      end do
!$omp end do nowait
!$omp do private(lt,nr,j,ip)
      do lt = 1, n_jk_e
        do ip = 2, np_smp
          do nr = 1, nkrt
            j = nr + (lt-1)*nkrt
            Smat(1)%tor_e(j) = Smat(1)%tor_e(j) + Smat(ip)%tor_e(j)
          end do
        end do
      end do
!$omp end do nowait
!$omp do private(lt,nr,j,ip)
      do lt = 1, n_jk_o
        do ip = 2, np_smp
          do nr = 1, nkrs
            j = nr + (lt-1)*nkrs
            Smat(1)%pol_o(j) = Smat(1)%pol_o(j) + Smat(ip)%pol_o(j)
          end do
        end do
      end do
!$omp end do nowait
!$omp do private(lt,nr,j,ip)
      do lt = 1, n_jk_o
        do ip = 2, np_smp
          do nr = 1, nkrt
            j = nr + (lt-1)*nkrt
            Smat(1)%tor_o(j) =  Smat(1)%tor_o(j) + Smat(ip)%tor_o(j)
          end do
        end do
      end do
!$omp end do
!$omp end parallel
!
      end subroutine sum_spectr_over_kr_in
!
! -----------------------------------------------------------------------
!
      subroutine sum_spectr_over_smp_out                                &
     &         (nkrs, nkrt, n_jk_e, n_jk_o, Smat)
!
      integer(kind = kint), intent(in) :: nkrs, nkrt
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o
      type(spectr_matrix_omp), intent(inout) :: Smat(np_smp)
!
      integer(kind = kint) :: ip
!
!
!$omp parallel private(ip)
        do ip = 2, np_smp
!$omp workshare
          Smat(1)%pol_e(1:nkrs*n_jk_e) =  Smat(1)%pol_e(1:nkrs*n_jk_e)  &
     &       + Smat(ip)%pol_e(1:nkrs*n_jk_e)
!$omp end workshare nowait
!$omp workshare
          Smat(1)%tor_e(1:nkrt*n_jk_e) =  Smat(1)%tor_e(1:nkrt*n_jk_e)  &
     &       + Smat(ip)%tor_e(1:nkrt*n_jk_e)
!$omp end workshare nowait
!$omp workshare
          Smat(1)%pol_o(1:nkrs*n_jk_o) = Smat(1)%pol_o(1:nkrs*n_jk_o)   &
     &       + Smat(ip)%pol_o(1:nkrs*n_jk_o)
!$omp end workshare nowait
!$omp workshare
          Smat(1)%tor_o(1:nkrt*n_jk_o) = Smat(1)%tor_o(1:nkrt*n_jk_o)   &
     &       + Smat(ip)%tor_o(1:nkrt*n_jk_o)
!$omp end workshare nowait
        end do
!$omp end parallel
!
      end subroutine sum_spectr_over_smp_out
!
! -----------------------------------------------------------------------
!
      end module sum_spectr_over_smp_segment
