+++
title = "Continuing Sudoku"
slug = "continuing-sudoku"
date = "2007-01-20T14:58:00+00:00"
draft = false

+++

Your mission, should you choose to accept it, is to explain what the following code does:

<code>

    class Amb
    def initialize
    @error = Exception.new("Ran out of possibilities")
    @failure_continuation = lambda {|v| @error}
    end

    def assert(assertion)
    if !assertion
    self.fail
    end
    end

    def deny(assertion)
    assert !assertion
    end

    def fail
    @failure_continuation.call(nil)
    end

    def maybe
    one_of [true, false]
    end

    def one_of(collection = [])
    k_prev = @failure_continuation
          callcc do |k_entry|
            collection.each do |item|
              callcc do |k_next|
                @failure_continuation = lambda do |v|
                  @failure_continuation = k_prev
                  k_next.call(v)
                end
                k_entry.call(item)
              end
            end
            result = k_prev.call(nil)
            if result == @error
              raise @error.message
            end
          end
        end

        def all_values(&a_block)
          k_prev = @failure_continuation
          results = []
          callcc do |k_retry|
            @failure_continuation = lambda {|v| k_retry.call(false)}
            results << a_block.call
            k_retry.call(true)
          end && fail
          @failure_continuation = k_prev
          results
        end
      end

</code>
Easy? Now explain how it does it.
