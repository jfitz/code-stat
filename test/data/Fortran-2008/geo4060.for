MODULE class_terrain
    IMPLICIT NONE
    PRIVATE

    TYPE , PUBLIC :: terrain
        CHARACTER(LEN=80) :: filename
        INTEGER , POINTER :: map(:,:) => null()
        INTEGER :: n_cols = 0
        INTEGER :: n_rows = 0
        INTEGER :: cell_size = 0
        INTEGER :: no_value = 0

        CONTAINS
            PROCEDURE :: load => load_data
            PROCEDURE :: dump => dump_data
    END TYPE terrain

    CONTAINS
        SUBROUTINE load_data(this)
            CLASS(terrain) :: this
            INTEGER , DIMENSION(4) :: vars
            INTEGER , PARAMETER :: u_number = 10
            CHARACTER(LEN=512),DIMENSION(4) :: buffer
            INTEGER :: b_start, b_end
            INTEGER :: i, j, k, l, res
            INTEGER , EXTERNAL :: a2i

            OPEN(UNIT=u_number,FILE=this%filename,FORM=’FORMATTED’,IOSTAT=res)
            buffer = ’ ’
            DO i = 1, 4
                READ(UNIT=u_number,FMT=’(A)’,IOSTAT=res)
                buffer(i)
                IF(res /= 0) THEN
                    PRINT*, ’Error in reading line 1, status ’, res
                    CLOSE(UNIT=u_number)
                    RETURN
                END IF
            END DO
            
            CALL extract_header_lines(buffer,vars)
            this%n_cols = vars(1)
            this%n_rows = vars(2)
            this%cell_size = vars(3)
            this%no_value = vars(4)
            ALLOCATE(this%map(this%n_rows,this%n_cols),STAT=res)
            IF(res /= 0) THEN
                PRINT *, ’Allocationfailure, status ’, res
                CLOSE(UNIT=u_number)
            END IF 
        END SUBROUTINE load_data

        SUBROUTINE dump_data(this)
            CLASS(terrain) :: this
            INTEGER , PARAMETER :: u_number = 11
            INTEGER :: i, j, res
            
            OPEN(UNIT=u_number,FILE=this%filename,FORM=’FORMATTED’,IOSTAT=res)
            WRITE(UNIT=u_number,FMT=’(I5)’,IOSTAT=res) this%n_cols
            WRITE(UNIT=u_number,FMT=’(I5)’,IOSTAT=res) this%n_rows
            WRITE(UNIT=u_number,FMT=’(I5)’,IOSTAT=res) this%cell_size
            WRITE(UNIT=u_number,FMT=’(I5)’,IOSTAT=res) this%no_value
            DO i = 1, this%n_rows
                DO j = 1, this%n_cols
                    WRITE(UNIT=u_number,FMT=’(I4,A2)’,ADVANCE=’NO’,IOSTAT=res) &
                    this%map(i,j), ’, ’
                END DO
                WRITE(UNIT=u_number,FMT=’(A)’,IOSTAT=res) ’ ’
            END DO
            CLOSE(UNIT=u_number)
        END SUBROUTINE dump_data

END MODULE class_terrain

PROGRAM ctest
    USE class_terrain
    IMPLICITNONE
    TYPE(terrain) :: td1
 
    td1 = terrain(’demfile.dat’)
    CALL td1%fload()
END PROGRAM ctest
